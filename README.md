# inventario-ra2
---
Pontifícia Universidade Católica do Paraná

Programação Lógica e Funcional

Docente: Frank Coelho de Alcantara

DIcentes: 

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
