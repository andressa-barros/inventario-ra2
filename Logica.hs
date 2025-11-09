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
-- verifica se o ID já existe:
-- se sim: erro, se nao: insere o item no mapa, cria log de sucesso e retorna Right

addItem :: UTCTime -> Item -> Inventario -> Either String ResultadoOperacao
addItem horario item inventario = case Map.lookup (itemID item) inventario of
    Just _ -> Left "id já existe"
    Nothing ->
        let novoInventario = Map.insert (itemID item) item inventario
            logEntrada = LogEntry horario Add ("add" ++ show (quantidade item) ++ "x " ++ itemID item) Sucesso
        in Right (novoInventario, logEntrada)

-- removeItem, remove certa qntd de um item
-- Map.lookup acha item:
-- se n achar: erro, se achar: confere se qntd > 0 e < que estoque atual,
-- se td certo: cria nv versao do item com qntd reduzida, att mapa e monta log


removeItem :: UTCTime -> String -> Int -> Inventario -> Either String ResultadoOperacao
removeItem horario itemId quantidadeRemovida inventario = case Map.lookup itemId inventario of
    Nothing -> Left "id não encontrado"
    Just item ->
        if quantidadeRemovida <= 0 then Left "quantidade invalida"
        else if quantidadeRemovida > quantidade item then Left "estoque insuficiente"
        else
            let itemAtualizado = item { quantidade = quantidade item - quantidadeRemovida }
                novoInventario = Map.insert itemId itemAtualizado inventario
                logEntrada = LogEntry horario Remove ("remove " ++ show quantidadeRemovida ++ "x " ++ itemId) Sucesso
            in Right (novoInventario, logEntrada)