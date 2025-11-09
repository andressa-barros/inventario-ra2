-- regras de negocio puras

-- p quem for fzr as outras partes do projeto importarem só o q interessa :P
module Logica
    ( ResultadoOperacao
    , addItem
    , removeItem
    , updateQty
    ) where

import Tipos
import qualified DataMap as Map
import Data.Time (UTCTime) -- p poder escrever nas assinaturas

-- se retorno de sucesso = (novo inventario + log)
type ResultadoOperacao = (Inventario, LogEntry)