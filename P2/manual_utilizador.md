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

O objetivo do programa é permitir jogar uma partida de Adji-boto num de dois modos:

- Humano vs Computador
- Computador vs Computador

## 2. Descrição Geral

O programa funciona da seguinte forma:

1. O utilizador indica qual o modo de jogo (Humano vs Computador ou Computador vs Computador);

Modo Humano vs Computador:

1. O utilizador indica se quer começar primeiro
2. A cada jogada do utilizador é pedida qual a coluna onde quer jogar
3. O jogo alterna entre jogadas do utilizador e do computador até o jogo acabar

No modo Computador vs Computador, é simulado um jogo até este acabar.

## 3. Utilização do Programa

Para utilizar o programa o utilizador segue os seguintes passos:

1. Invocar a função `iniciar` na janela Listener do LispWorks.

```lisp
CL-USER 1 > (iniciar)
```

2. O utilizador indica qual o modo de jogo (Humano vs Computador ou Computador vs Computador)

```lisp
Escolha o modo de jogo (1-Humano vs Computador; 2-Computador vs Computador):
```

3. Caso escolha o modo Humano vs Computador, é de seguida solicitado se o utilizador quer começar primeiro.

```lisp
Quer iniciar primeiro? (1-Sim; 2-Não):
```

4. Para cada jogada do utilizador é apresentado o tabuleiro atual e é pedida a coluna onde quer jogar

```lisp
TABULEIRO ATUAL:
8 8 8 8 8 8
8 8 8 8 8 8
--------------------------------------
Turno do jogador humano (1).
Indique a coluna onde pretende jogar (entre 1 e 6):
```

5. Depois de cada jogada (humano ou computador) é apresentada a jogada efetuada, o novo estado do tabuleiro, a pontuação e algumas métricas

```lisp
TABULEIRO ATUAL:
8 8 8 8 8 8
8 8 8 8 8 8
--------------------------------------
Turno do jogador humano (1).
Indique a coluna onde pretende jogar (entre 0 e 5): 0
Jogada (0 0):
0 8 8 8 9 9
9 9 9 9 9 9
Pontuação: [Jogador 1 - 0] | [Jogador 2 - 0] 
Nos analisados: 722.0 | Cortes: 167.0 | Tempo gasto (s): 191/125
--------------------------------------
```

6. Caso o utilizador escolha uma coluna sem peças aparece um erro e pode escolher uma nova coluna

```lisp
TABULEIRO ATUAL:
11 0 23 0 4 7
9 0 15 11 5 3
--------------------------------------
Turno do jogador humano (1).
Indique a coluna onde pretende jogar (entre 0 e 5): 1
A coluna escolhida não é válida ou está vazia. Por favor, escolha outra.
Indique a coluna onde pretende jogar (entre 0 e 5): 0
Jogada (0 0):
0 1 24 1 5 8
10 1 16 12 6 4
Pontuação: [Jogador 1 - 1] | [Jogador 2 - 7] 
Nos analisados: 0.0 | Cortes: 0.0 | Tempo gasto (s): 5429/1000
--------------------------------------
```

7. Quando não existirem jogadas disponíveis para um dos jogadores a sua vez é passada

```lisp
TABULEIRO ATUAL:
0 0 0 0 0 0
0 0 0 0 3 5
--------------------------------------
Turno do jogador humano (1).
Não há mais jogadas disponíveis para o jogador humano (1).
```

8. Quando o tabuleiro ficar vazio o jogo acaba e é apresentado o vencedor

```lisp
TABULEIRO ATUAL:
0 0 0 0 0 0
0 0 0 0 0 1
--------------------------------------
Turno do jogador computador (2).
Jogada (1 5):
0 0 0 0 0 0
0 0 0 0 0 0
Pontuação: [Jogador 1 - 33] | [Jogador 2 - 63] 
Nos analisados: 0.0 | Cortes: 0.0 | Tempo gasto (s): 0.0
--------------------------------------


O jogador computador (2) venceu!
```

No modo Computador VS Computador, o utilizador não precisa de introduzir nenhum comando visto que quano iniciar o jogo ambos os jogadores irão escolher as suas jogadas até o tabuleiro ficar vazio.

## 4. Informação Necessária e Produzida

### 4.1. Informação Necessária

Nome | Descrição
-----|----------
Modo de Jogo | Modo de jogo escolhido (Humano vs Computador ou Computador vs Computador)
Primeiro jogador | Quem começa a jogar no modo Humano vs Computador
Coluna escolhida | Qual a coluna onde o utilizador irá realizar a sua jogada

### 4.2. Informação Produzida

Nome | Descrição
-----|----------
Medidas de desempenho | Nº de nós analisados, comprimento do caminho e tempo de execução
log.dat | Ficheiro de log onde são apresentadas todas as jogadas efetuadas e as respetivas medidas de desempenho

## Limitações

Devido ao limite de memória do IDE não é possível aumentar a profundidade do algoritmo Negamax para além de 4 níveis.
