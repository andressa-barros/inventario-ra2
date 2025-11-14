module Relatorios
  ( historicoPorItem
  , logsDeErro
  , itemMaisMovimentado
  ) where

import Tipos
import Data.List (sort, group)

historicoPorItem :: String -> [LogEntry] -> [LogEntry]
historicoPorItem idItem logs =
  filter (\l -> idItem `elem` words (detalhes l)) logs

logsDeErro :: [LogEntry] -> [LogEntry]
logsDeErro logs =
  filter (\l -> case status l of
                  Falha _ -> True
                  _       -> False) logs

itemMaisMovimentado :: [LogEntry] -> Maybe (String, Int)
itemMaisMovimentado logs =
  let
      okLogs = filter (\l -> case status l of Sucesso -> True; _ -> False) logs

      nomes = [ last ws | l <- okLogs
                        , let ws = words (detalhes l)
                        , not (null ws)
              ]

      -- Aqui tratamos padrão vazio só por segurança
      contaLista []     = Nothing
      contaLista (x:xs) = Just (x, 1 + length xs)

      agrupado = map contaLista (group (sort nomes))

      -- remove os Nothings (não deve ter nenhum)
      pares = [ p | Just p <- agrupado ]

      pegaMaior [] = Nothing
      pegaMaior (p:ps) = Just (foldl (\a b -> if snd b > snd a then b else a) p ps)

  in pegaMaior pares
