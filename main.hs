import Tipos

main :: IO ()
main = do
  let itemTeste = Item "001" "Teclado" 5 "Periférico"
  let texto = show itemTeste
  putStrLn ("Convertido em texto: " ++ texto)

  let itemLido = read texto :: Item
  putStrLn ("Lido de volta: " ++ show itemLido)
