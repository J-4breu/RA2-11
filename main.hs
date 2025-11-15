{-# LANGUAGE BangPatterns #-}

import qualified Data.Map as Map
import Data.Map (Map)
import Data.Time
import System.IO
import Control.Exception (catch, SomeException)
import Data.List (intercalate)
import System.IO.Unsafe (unsafePerformIO)

-- ============================================================================
-- TIPOS DE DADOS (Aluno 1: Arquiteto de Dados)
-- ============================================================================

-- | Representa um item no inventário com identificador, nome, quantidade e categoria
data Item = Item
  { itemID :: String      -- ^ Identificador único do item
  , nome :: String        -- ^ Nome descritivo do item
  , quantidade :: Int     -- ^ Quantidade em estoque
  , categoria :: String   -- ^ Categoria do item
  } deriving (Show, Read, Eq)

-- | Inventário como mapa de itemID para Item
type Inventario = Map String Item

-- | Tipo de ação realizada no sistema (ADT)
data AcaoLog = Add        -- ^ Adição de item
             | Remove     -- ^ Remoção de item
             | Update     -- ^ Atualização de quantidade
             | QueryFail  -- ^ Falha em consulta
             | ListItems  -- ^ Listagem de itens
             | Report     -- ^ Geração de relatório
  deriving (Show, Read, Eq)

-- | Status do resultado da operação (ADT)
data StatusLog = Sucesso           -- ^ Operação bem-sucedida
               | Falha String      -- ^ Operação falhou com mensagem
  deriving (Show, Read, Eq)

-- | Entrada no log de auditoria
data LogEntry = LogEntry
  { timestamp :: UTCTime    -- ^ Momento da operação
  , acao :: AcaoLog         -- ^ Tipo de ação executada
  , detalhes :: String      -- ^ Descrição detalhada
  , status :: StatusLog     -- ^ Resultado da operação
  } deriving (Show, Read, Eq)
-- ============================================================================
-- FUNÇÕES DE ANÁLISE DE LOGS (Aluno 4: Validação e Documentação)
-- ============================================================================

-- | Filtra apenas os logs que representam erros
logsDeErro :: [LogEntry] -> [LogEntry]
logsDeErro = filter isErro
  where
    isErro (LogEntry _ _ _ (Falha _)) = True
    isErro _ = False

-- | Retorna histórico de operações que mencionam um item específico
historicoPorItem :: String -> [LogEntry] -> [LogEntry]
historicoPorItem itemId = filter contemItem
  where
    contemItem (LogEntry _ _ det _) = itemId `elem` words det

-- | Identifica o item mais movimentado (mais menções nos logs)
itemMaisMovimentado :: [LogEntry] -> Maybe (String, Int)
itemMaisMovimentado logs =
  let
    -- Extrai IDs de itens dos logs
    items = concatMap extrairItems logs
    -- Conta ocorrências de cada item
    contagens = Map.toList $ Map.fromListWith (+) [(i, 1) | i <- items]
  in
    if null contagens
      then Nothing
      else Just $ foldr1 maiorContagem contagens
  where
    -- Extrai primeiro ID mencionado nos detalhes
    extrairItems (LogEntry _ _ det _) = 
      take 1 $ filter (not . null) $ words det
    -- Compara e retorna item com maior contagem
    maiorContagem x y = if snd x > snd y then x else y
