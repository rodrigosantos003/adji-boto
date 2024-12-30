# Manual de Utilizador

> **UC**: Inteligência Artificial
>
> **Alunos:**
>
> - João Fernandes - 202100718
> - Rodrigo Santos - 202100722
>
> **Docente**: Joaquim Filipe

## Índice

1. [Objetivo](#1-objetivo)
2. [Descrição Geral](#2-descrição-geral)
3. [Utilização do Programa](#3-utilização-do-programa)
4. [Informação Necessária e Produzida](#4-informação-necessária-e-produzida)
5. [Limitações](#limitações)

## 1. Objetivo

O objetivo do programa é resolver problemas do jogo Adji-boto, que consiste num tabuleiro com 2 linhas e 6 colunas.

## 2. Descrição Geral

O programa funciona da seguinte forma:

1. O utilizador indica qual o problema que pretende resolver;
2. O utilizador indica qual o algoritmo que pretende usar para resolver o problema;
3. O utilizador fornece parâmetros adicionais para o algoritmo indicado, caso necessário;
4. O programa realiza a procura do caminho e devolve a solução;
5. A solução é apresentada caso seja encontrada. Caso contrário é apresentada uma mensagem de erro

## 3. Utilização do Programa

Para utilizar o programa o utilizador segue os seguintes passos:

1. Invocar a função `iniciar` na janela Listener do LispWorks.

```lisp
CL-USER 1 > (iniciar)
```

2. O utilizador indica qual o problema que pretende resolver (uma letra de A a G).

```lisp
Escolha um problema para resolver (de A a G): 
```

3. Ao indicar o problema, é de seguida solicitado o algoritmo a utilizar (BFS, DFS ou A*).

```lisp
Escolha o algoritmo a executar (BFS, DFS ou A-STAR): 
```

4. Por fim é apresentado o resultado do problema com o algoritmo.

```lisp
Executando BFS...
RESULTADO
Estado inicial:  "((0 0 0 0 0 2) (0 0 0 0 4 0))"
Caminho:
((0 0 0 0 0 2) (0 0 0 0 4 0)) - 0
((0 0 0 0 1 0) (0 0 0 0 4 0)) - 1
((0 0 0 0 2 1) (0 0 0 0 0 1)) - 2
((0 0 0 0 0 0) (0 0 0 0 0 1)) - 3
((0 0 0 0 0 0) (0 0 0 0 0 0)) - 4
Número nós: 25.0 
Comprimento caminho: 4 
Penetrância: 0.16 
Fator Ramificação: 2.5000 
Tempo Execução: 0.007 s 
```

Abaixo é representada uma utilização completa.

```lisp
CL-USER 1 > (iniciar)
Escolha um problema para resolver (de A a G): A
Problema encontrado: A
Escolha o algoritmo a executar (BFS, DFS ou A-STAR): bfs
Executando BFS...
RESULTADO
Estado inicial:  "((0 0 0 0 0 2) (0 0 0 0 4 0))"
Caminho:
((0 0 0 0 0 2) (0 0 0 0 4 0)) - 0
((0 0 0 0 1 0) (0 0 0 0 4 0)) - 1
((0 0 0 0 2 1) (0 0 0 0 0 1)) - 2
((0 0 0 0 0 0) (0 0 0 0 0 1)) - 3
((0 0 0 0 0 0) (0 0 0 0 0 0)) - 4
Número nós: 25.0 
Comprimento caminho: 4 
Penetrância: 0.16 
Fator Ramificação: 2.5000 
Tempo Execução: 0.007 s 
```

## 4. Informação Necessária e Produzida

### 4.1. Informação Necessária

Nome | Descrição
-----|----------
problemas.dat| Ficheiro onde serão criados e obtidos problemas
Problema | Letra do problema a resolver
Algoritmo | Sigla do algoritmo a utilizador
Profundidade| Profundidade máxima para o algoritmo DFS (opcional)
Heurística | Nome da função heurística para o algoritmo A*

### 4.2. Informação Produzida

Nome | Descrição
-----|----------
Caminho | Caminho para a solução do problema indicado
Medidas de desempenho | Nº de nós gerados, comprimento do caminho, penetrância, fator de ramificação média e tempo de execução

## Limitações

Não é possível resolver os problemas de B a G usando o algoritmo BFS (não aumentando a quantidade de memória).
