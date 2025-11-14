# inventario-ra2
---
Pontifícia Universidade Católica do Paraná

Programação Lógica e Funcional

Docente: Frank Coelho de Alcantara

Dicentes: 

1 - Andressa de Oliveira Barros @andressa-barros

2 - Fernanda Costa Moraes @Fer9211

3 - Lissa Deguti @lilidgt

4 - Messissa Weiss Perussulo @Missywp

---

## Como compilar e executar o projeto
Compilação: use o comadno ghc main.hs
Execução: use o comando .\Main.exe, ele tentará carregar Inventario.dat e Auditoria.log. Na primeira execução, criará estes arquivos.

O programa entrará no loop principal, aguardando os comandos algum dos seguintes comandos:

| Comando | Formato | Descrição |
| :--- | :--- | :--- |
| **Adicionar** | `add <id> <nome> <qtd> <categoria>` | Adiciona um novo item ao inventário. |
| **Remover** | `remove <id> <qtd>` | Remove uma quantidade do item existente. |
| **Atualizar** | `update <id> <nova_qtd>` | Define uma nova quantidade total para o item. |
| **Listar** | `listar` | Exibe o inventário atual (implementado em `main.hs`). |
| **Relatório** | `report` | [cite_start]Gera e exibe relatórios baseados no `Auditoria.log`[cite: 61]. |
| **Sair** | `sair` | Encerra o programa. |

---

## Cenário 1: Persistência de Estado (Sucesso)

O programa foi iniciado, os arquivos Inventario.dat e Auditoria.log não existiam, e o sistema iniciou com sucesso.

Foram dados os comandos para adicionar os três itens:

  1 - add 001 mouse 20 Periferico

  2 - add 002 teclado 10 Periferico

  3 - add 003 Caneta 13 Papelaria

O programa foi encerrado e inicalizado novamente, ao reiniciar leu os arquivos Inventario.dat e Auditoria.log.

O comando listar exibiu os três itens adicionados:

  ID: 001 | Nome: mouse | Quantidade: 20 | Categoria: Periferico
  
  ID: 002 | Nome: teclado | Quantidade: 10 | Categoria: Periferico
  
  ID: 003 | Nome: Caneta | Quantidade: 13 | Categoria: Papelaria

## Cenário 2: Erro de Lógica (Estoque Insuficiente)

Com o comando add 004 Teclado 10 Periferico foram adicionados 10 teclados ao sistema.

Na tentativa de remover 15 unidades do item o sistema apresentou a mensagem de erro "Erro: estoque insuficiente". 

O arquivo Inventario.dat ainda consta 10 unidades do item: "{itemID = "004", nome = "Teclado", quantidade = 10, categoria = "Periferico"}"

O arquivo Auditoria consta o erro apresentado: "LogEntry {timestamp = 2025-11-14 18:54:21.327336803 UTC, acao = QueryFail, detalhes = "estoque insuficiente", status = Falha "estoque insuficiente"}"

## Cenário 3: Geração de Relatório de Erros

Ao executar o report ele apresentou o erro do cenário anterior: 

"1. Logs de erro:
LogEntry {timestamp = 2025-11-14 19:19:37.972830127 UTC, acao = QueryFail, detalhes = "estoque insuficiente", status = Falha "estoque insuficiente"}

2. Item mais movimentado:
Just ("011",1)"

O relatório exibiu a entrada de log de erro (estoque insuficiente). Além disso, a função itemMaisMovimentado ignorou o log de erro e retornou o item mais movimentado baseado nas transações de sucesso (ex: Just ("011", 1)).

