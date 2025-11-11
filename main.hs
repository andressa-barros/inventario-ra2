module Main where

import Tipos
import Logica
import qualified Data.Map as Map
import Data.Time (getCurrentTime, UTCTime)
import Control.Exception (catch, IOException)
import System.IO (readFile, writeFile)
import System.IO.Error (isDoesNotExistError)

arquivoInventario :: FilePath
arquivoInventario = "Inventario.dat"

arquivoLog :: FilePath
arquivoLog = "Auditoria.log"

carregarInventario :: IO Inventario
carregarInventario = catch
  (do 
    conteudo <- readFile arquivoInventario
    return (read conteudo :: Inventario)) --read converte a string pro tipo inventario
  (\e -> 
    if isDoesNotExistError e then do
      putStrLn "Inventario.dat não encontrado. Criando inventário vazio."
      return Map.empty --inventário vazio
    else do
      putStrLn $ "Erro ao ler inventário: " ++ show (e :: IOException)
      return Map.empty)

carregarLog :: IO [LogEntry]
carregarLog = catch
  (do
    conteudo <- readFile arquivoLog
    return (read conteudo :: [LogEntry]))
  (\e ->
    if isDoesNotExistError e then do
      putStrLn "Auditoria.log não encontrado. Criando log vazio."
      return [] --lista de logs vazia
    else do
      putStrLn $ "Erro ao ler log: " ++ show (e :: IOException)
      return [])

salvarInventario :: Inventario -> IO ()
salvarInventario inv = writeFile arquivoInventario (show inv)

salvarLogs :: [LogEntry] -> IO ()
salvarLogs logs = writeFile arquivoLog (show logs)

--erros de lógica
registrarFalha :: String -> Inventario -> [LogEntry] -> IO ()
registrarFalha msg inv logs = do
  putStrLn $ "Erro: " ++ msg
  loop inv logs

--processa comandos e atualiza
loop :: Inventario -> [LogEntry] -> IO ()
loop inv logs = do
  putStr "\nComando (add, remove, update, listar, report, sair): "
  comando <- getLine
  case words comando of

    ("add":id_:nome:qtdStr:cat:_) -> 
      case reads qtdStr of --ler a quantidade 
        [(qtd, "")] -> do
          now <- getCurrentTime
          let item = Item id_ nome qtd cat
          case addItem now item inv of --lógica para adicionar
            Left err -> registrarFalha err inv logs
            Right (novoInv, logEntrada) -> do
              putStrLn "Item adicionado."
              let novosLogs = logs ++ [logEntrada]
              salvarInventario novoInv --novo estado
              salvarLogs novosLogs
              loop novoInv novosLogs --continua com o novo estado
        _ -> registrarFalha "Quantidade inválida." inv logs

    ("remove":id_:qtdStr:_) ->
      case reads qtdStr of
        [(qtd, "")] -> do
          now <- getCurrentTime
          case removeItem now id_ qtd inv of --lógica para remover
            Left err -> registrarFalha err inv logs
            Right (novoInv, logEntrada) -> do
              putStrLn "Item removido."
              let novosLogs = logs ++ [logEntrada]
              salvarInventario novoInv
              salvarLogs novosLogs
              loop novoInv novosLogs
        _ -> registrarFalha "Quantidade inválida." inv logs

    ("update":id_:qtdStr:_) ->
      case reads qtdStr of
        [(qtd, "")] -> do
          now <- getCurrentTime
          case updateQty now id_ qtd inv of --lógica para atualizar
            Left err -> registrarFalha err inv logs
            Right (novoInv, logEntrada) -> do
              putStrLn "Quantidade atualizada."
              let novosLogs = logs ++ [logEntrada]
              salvarInventario novoInv
              salvarLogs novosLogs
              loop novoInv novosLogs
        _ -> registrarFalha "Quantidade inválida." inv logs

    ("listar":_) -> do
      putStrLn "\nInventário atual:"
      mapM_ (putStrLn . formatItem) (Map.elems inv) --lista os itens
      loop inv logs

    ("report":_) -> do
      putStrLn "\nÚltimos 5 registros do log:"
      mapM_ print (take 5 (reverse logs)) --mais recentes
      loop inv logs

    ("sair":_) -> putStrLn "Encerrando programa..." 

    _ -> do
      putStrLn "Comando inválido."
      loop inv logs

--formatar pra exibição
formatItem :: Item -> String
formatItem (Item id_ nome qtd cat) =
  "ID: " ++ id_ ++ " | Nome: " ++ nome ++ " | Quantidade: " ++ show qtd ++ " | Categoria: " ++ cat

main :: IO ()
main = do
  inventario <- carregarInventario
  logs <- carregarLog
  putStrLn "Inventário"
  loop inventario logs