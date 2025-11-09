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

-- addItem, adiciona um item nv ao inventario
-- verifica se o ID já existe,
-- se sim: erro, se nao: insere o item no mapa, cria log de sucesso e retorna Right

addItem :: UTCTime -> Item -> Inventario -> Either String ResultadoOperacao
addItem horario item inventario = case Map.lookup (itemID item) inventario of
    Just _ -> Left "id já existe"
    Nothing ->
        let novoInventario = Map.insert (itemID item) item inventario
            logEntrada = LogEntry horario Add ("add" ++ show (quantidade item) ++ "x " ++ itemID item) Sucesso
        in Right (novoInventario, logEntrada)

        