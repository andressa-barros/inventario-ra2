module Main where

import Tipos
import Logica
import qualified Data.Map as Map
import Data.Time (getCurrentTime)
import Control.Exception (catch, IOException)
import System.IO (readFile, writeFile, appendFile)
import System.IO.Error (isDoesNotExistError)
import Relatorios

arquivoInventario :: FilePath
arquivoInventario = "Inventario.dat"
arquivoLog :: FilePath
arquivoLog = "Auditoria.log"

-- Carregar inventário
carregarInventario :: IO Inventario
carregarInventario = catch
  (do 
    conteudo <- readFile arquivoInventario
    return (read conteudo :: Inventario))
  (\e -> 
    if isDoesNotExistError e then do
      putStrLn "Inventario.dat não encontrado. Criando inventário vazio."
      return Map.empty
    else do
      putStrLn $ "Erro ao ler inventário: " ++ show (e :: IOException)
      return Map.empty)

-- Carregar logs
carregarLog :: IO [LogEntry]
carregarLog = catch
  (do
    conteudo <- readFile arquivoLog
    return (read conteudo :: [LogEntry]))
  (\e ->
    if isDoesNotExistError e then do
      putStrLn "Auditoria.log não encontrado. Criando log vazio."
      return []
    else do
      putStrLn $ "Erro ao ler log: " ++ show (e :: IOException)
      return [])

-- Salvar inventário
salvarInventario :: Inventario -> IO ()
salvarInventario inv = writeFile arquivoInventario (show inv)

-- Registrar falha de lógica (append)
registrarFalha :: String -> Inventario -> [LogEntry] -> IO [LogEntry]
registrarFalha msg _ logs = do
  now <- getCurrentTime
  let logErro = LogEntry now QueryFail msg (Falha msg)
      novosLogs = logs ++ [logErro]
  appendFile arquivoLog (show logErro ++ "\n")
  putStrLn $ "Erro: " ++ msg
  return novosLogs

-- Registrar sucesso (append)
registrarSucesso :: LogEntry -> [LogEntry] -> IO [LogEntry]
registrarSucesso logEntrada logs = do
  appendFile arquivoLog (show logEntrada ++ "\n")
  return (logs ++ [logEntrada])

-- Loop principal de comandos
loop :: Inventario -> [LogEntry] -> IO ()
loop inv logs = do
  putStr "\nComando (add, remove, update, listar, report, sair): "
  comando <- getLine
  case words comando of

    -- ADD
    ("add":id_:nome:qtdStr:cat:_) ->
      case reads qtdStr of
        [(qtd, "")] -> do
          now <- getCurrentTime
          let item = Item id_ nome qtd cat
          case addItem now item inv of
            Left err -> do
              novosLogs <- registrarFalha err inv logs
              loop inv novosLogs
            Right (novoInv, logEntrada) -> do
              putStrLn "Item adicionado."
              novosLogs <- registrarSucesso logEntrada logs
              salvarInventario novoInv
              loop novoInv novosLogs
        _ -> do
          novosLogs <- registrarFalha "Quantidade inválida." inv logs
          loop inv novosLogs

    -- REMOVE
    ("remove":id_:qtdStr:_) ->
      case reads qtdStr of
        [(qtd, "")] -> do
          now <- getCurrentTime
          case removeItem now id_ qtd inv of
            Left err -> do
              novosLogs <- registrarFalha err inv logs
              loop inv novosLogs
            Right (novoInv, logEntrada) -> do
              putStrLn "Item removido."
              novosLogs <- registrarSucesso logEntrada logs
              salvarInventario novoInv
              loop novoInv novosLogs
        _ -> do
          novosLogs <- registrarFalha "Quantidade inválida." inv logs
          loop inv novosLogs

    -- UPDATE
    ("update":id_:qtdStr:_) ->
      case reads qtdStr of
        [(qtd, "")] -> do
          now <- getCurrentTime
          case updateQty now id_ qtd inv of
            Left err -> do
              novosLogs <- registrarFalha err inv logs
              loop inv novosLogs
            Right (novoInv, logEntrada) -> do
              putStrLn "Quantidade atualizada."
              novosLogs <- registrarSucesso logEntrada logs
              salvarInventario novoInv
              loop novoInv novosLogs
        _ -> do
          novosLogs <- registrarFalha "Quantidade inválida." inv logs
          loop inv novosLogs

    -- LISTAR
    ("listar":_) -> do
      putStrLn "\nInventário atual:"
      mapM_ (putStrLn . formatItem) (Map.elems inv)
      loop inv logs

    -- REPORT
    ("report":_) -> do
      putStrLn "\nDigite o ID do item para visualizar o histórico:"
      idItem <- getLine
      putStrLn ("\nHistórico do item " ++ idItem ++ ":")
      mapM_ print (historicoPorItem idItem logs)
      putStrLn "\nItem mais movimentado:"
      print (itemMaisMovimentado logs)
      loop inv logs

    -- SAIR
    ("sair":_) -> putStrLn "Encerrando programa..."

    -- COMANDO INVÁLIDO
    _ -> do
      putStrLn "Comando inválido."
      loop inv logs

-- Formatação de item para exibição
formatItem :: Item -> String
formatItem (Item id_ nome qtd cat) =
  "ID: " ++ id_ ++ " | Nome: " ++ nome ++ " | Quantidade: " ++ show qtd ++ " | Categoria: " ++ cat

-- Função principal
main :: IO ()
main = do
  inventario <- carregarInventario
  logs <- carregarLog
  putStrLn "Inventário"
  loop inventario logs 