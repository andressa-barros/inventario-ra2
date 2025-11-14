module Relatorios
  ( historicoPorItem
  , logsDeErro
  , itemMaisMovimentado
  ) where

import Tipos
import Data.List (sort, group)

-- Filtra os logs que têm o ID do item no texto
historicoPorItem :: String -> [LogEntry] -> [LogEntry]
historicoPorItem idItem logs =
  filter (\l -> idItem `elem` words (detalhes l)) logs

-- Retorna apenas os logs que são falhas
logsDeErro :: [LogEntry] -> [LogEntry]
logsDeErro logs =
  filter (\l -> case status l of
                  Falha _ -> True
                  _       -> False) logs

-- Encontra o item mais movimentado
itemMaisMovimentado :: [LogEntry] -> Maybe (String, Int)
itemMaisMovimentado logs =
  let
      -- só pega logs que deram certo
      okLogs = filter (\l -> case status l of Sucesso -> True; _ -> False) logs

      -- pega a última palavra do texto (que é o nome do item)
      nomes = [ last (words (detalhes l))
              | l <- okLogs
              , not (null (words (detalhes l)))
              ]

      -- agrupa iguais e conta
      agrupado = map (\x -> (head x, length x)) (group (sort nomes))

      -- função simples para pegar o maior
      pegaMaior [] = Nothing
      pegaMaior xs = Just (foldl1 (\a b -> if snd a >= snd b then a else b) xs)

  in pegaMaior agrupado
