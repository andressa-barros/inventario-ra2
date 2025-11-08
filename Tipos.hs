--Isso permite que outros arquivos importem esse módulo usando import Tipos.
module Tipos where

--IMPORTS
--Map é a estrutura que será usada para guardar os itens.
import Data.Map (Map)

--UTCTime serve para salvar o horário no log.
import Data.Time (UTCTime)

--Criar o tipo Item
data Item = Item
  { itemID     :: String
  , nome       :: String
  , quantidade :: Int
  , categoria  :: String
  } deriving (Show, Read, Eq) -- Show/Read permitem salvar e carregar os dados do arquivo.



--Define o inventário

--O inventário é simplesmente um mapa de IDs para Itens
type Inventario = Map String Item

--Criar o Tipo das Ações que entram no Log
data AcaoLog
  = Add
  | Remove
  | Update
  | QueryFail
  deriving (Show, Read, Eq)

--Criar o Tipo de Status da operação
data StatusLog
  = Sucesso
  | Falha String
  deriving (Show, Read, Eq)

--Criar o Tipo LogEntry
-- Registro da auditoria
data LogEntry = LogEntry
  { timestamp :: UTCTime
  , acao      :: AcaoLog
  , detalhes  :: String
  , status    :: StatusLog
  } deriving (Show, Read, Eq)

