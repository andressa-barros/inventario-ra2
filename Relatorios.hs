module Relatorios
  ( historicoPorItem
  , logsDeErro
  , itemMaisMovimentado
  ) where

import Tipos
import Data.List (sort, group, maximumBy)
import Data.Ord (comparing)

historicoPorItem :: String -> [LogEntry] -> [LogEntry]
historicoPorItem itemID = filter (\logEntry -> itemID `elem` words (detalhes logEntry))

logsDeErro :: [LogEntry] -> [LogEntry]
logsDeErro = filter (\logEntry -> case status logEntry of
                        Falha _ -> True
                        _ -> False)

itemMaisMovimentado :: [LogEntry] -> Maybe (String, Int)
itemMaisMovimentado logs =
  let nomes = [last (words (detalhes l)) | l <- logs, let ws = words (detalhes l), not (null ws)]
      agrupado = map (\xs -> (head xs, length xs)) . group . sort $ nomes
  in if null agrupado then Nothing else Just (maximumBy (comparing snd) agrupado)