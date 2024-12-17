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

1. [Introdução](#introdução)
2. [Descrição Geral](#descrição-geral)
3. [Utilização do Programa](#utilização-do-programa)
4. [Informação Necessária e Produzida](#informação-necessária-e-produzida)

## Introdução

No âmbito do primeiro projeto da Unidade Curricular (UC) de Inteligência Artificial (IA), do 1º semestre do 3º ano da Licenciatura em Engenharia Informática,
foi solicitado aos alunos que implementassem um programa para resolução do problema Adji-boto, recorrendo a algoritmos de procura de espaços de estados e usando a
linguagem de programação LISP.

O presente manual tem como objetivo descrever o funcionamento do programa.

## Descrição Geral

O programa funciona da seguinte forma:

1. O utilizador indica qual o problema que pretende resolver;
2. O utilizador indica qual o algoritmo que pretende usar para resolver o problema;
3. O utilizador fornece parâmetros adicionais para o algoritmo indicado, caso necessário;
4. O programa realiza a procura do caminho e devolve a solução;
5. A solução é apresentada caso seja encontrada. Caso contrário é apresentado NIL.

Podem ainda ser apresentadas as medidas de desempenho de um algoritmo, invocado a função `apresentar-desempenho`.

## Utilização do Programa

## Informação Necessária e Produzida

### Informação Necessária

Nome | Descrição
-----|----------
problemas.dat| Ficheiro onde serão criados e obtidos problemas
Problema | Letra do problema a resolver
Algoritmo | Sigla do algoritmo a utilizador
Profundidade| Profundidade máxima para o algoritmo DFS (opcional)
Heurística | Nome da função heurística para o algoritmo A*

### Informação Produzida

Nome | Descrição
-----|----------
Caminho | Caminho para a solução do problema indicado
