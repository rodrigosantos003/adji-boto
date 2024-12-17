# Manual Técnico

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
2. [Módulos](#módulos)
3. [Entidades](#entidades)
4. [Algoritmos](#algoritmos)
5. [Limitações e ideias para desenvolvimento futuro](#limitações-técnicas-e-ideias-para-desenvolvimento-futuro)
6. [Comparação dos Algortimos](#comparação-dos-algoritmos)
7. [Conclusão](#conclusão)

## Introdução

No âmbito do primeiro projeto da Unidade Curricular (UC) de Inteligência Artificial (IA), do 1º semestre do 3º ano da Licenciatura em Engenharia Informática,
foi solicitado aos alunos que implementassem um programa para resolução do problema Adji-boto, recorrendo a algoritmos de procura de espaços de estados e usando a
linguagem de programação LISP.

O presente relatório tem como objetivo descrever a solução desenvolvida pelos alunos, bem como os detalhes da sua implementação.

## Módulos

O sistema é composto pelos módulos apresentados na tabela abaixo.

Módulo | Descrição
-------|----------
Puzzle  | Funções auxiliares à resolução do problema em questão
Procura | Algoritmos de procura em espaço de estados (BFS, DFS e A*)
Desempenho| Medidas de desempenho dos algoritmos, nomeadamente Penetrância e Fator de Ramificação Média
Projeto | Interação com o utilizador, leitura e escrita em ficheiros e carregamento dos restantes módulos

## Entidades

Existem algumas entidades inerentes aos conceitos de procura em espaços de estados. A tabela abaixo descreve as entidades usadas no presente projeto.

Entidade| Descrição | Implementação
--------|-----------|---------------
Estado  | Estado atual do problema | Tabuleiro adji-boto, representado através de lista de listas
Nó      | Estado com informação adicional | Lista contendo o estado, nível, pai e custo
Operador| Função que altera o estado de um nó | Função de distribuição de peças de uma posição para as seguintes
Sucessor| Novo estado quando aplicado um operador a um nó | Novo nó com aplicação do operador ao nó pai.
Abertos | Lista com os nós que ainda não foram expandidos | Lista
Fechados| lista com os nós que já foram expandidos | Lista

## Algoritmos

De forma a resolver os problemas, os alunos implementaram os algoritmos Breadth-First Search, Depth-First Search e A*, usando uma abordagem funcional,
recursiva e não-destrutiva.

### BFS

O algoritmo BFS realiza uma procura em largura.
Na solução proposta pelos alunos, a função recebe como argumento o estado inicial do problema, a lista de abertos (inicializada com o nó do estado inicial) e a
lista de fechados (inicialmente vazia).

O seu funcionamento processa-se da seguinte forma:

1. Se a lista de abertos estiver vszia, retorna NIL, indicando que não existe solução;
2. Selecioina o primeiro nó da lista de abertos e gera os sucessores válidos deste nó, excluindo aqueles que já estão nas listas de abertos ou fechados;
3. Se algum dos sucessores for um nó objetivo, retorna o caminho até ter esse nó;
4. Se nenhum nó objetivo for encontrado, efetua a chamada recursiva com os sucessores válidos adicionadas ao final de abertos e o nó atual adicionado a fechados.

### DFS

O algoritmo DFS realiza uma procura em profundidade.
Na solução proposta pelos alunos, a função recebe como argumento o estado inicial do problema, um limite de profundidade (opcional), a lista de abertos
(inicializada com o nó do estado inicial) e a lista de fechados (inicialmente vazia).

O seu funcionamento processa-se da seguinte forma:

1. Se a lista de abertos estiver vazia, retorna NIL, indicando que não há solução;
2. Seleciona o primeiro nó da lista de abertos e verifica a profundidade do nó atual;
3. Se a profundidade do nó atual for maior ou igual ao limite, ignora este nó e contuna com o restante da lista de abertos;
4. Gera os sucessores válidos do nó atual, excluindo aqueles que já se encontram em abertos ou fechados;
5. Se algum dos sucessores for um nó objetivo, retorna o caminho até esse nó;
6. Se nenhum nó objetivo for encontrado, efetua a chamada recursiva com os sucessores válidos adicionados ao início de abertos e o nó atual adicoonado a fechados.

### A*

O algoritmo A* é um algoritmo heurístico, isto é, é fornecido conhecimento do domínio de aplicação para determinar se os nós gerados têm "interesse".
Na solução proposta pelos alunos, a função recebe como argumento o estado inicial do problema, uma função heurística, a lista de abertos
(inicializada com o nó do estado inicial) e a lista de fechados (inicialmente vazia).

O seu funcionamento processa-se da seguinte forma:

1. Se a lista de abertos estiver vazia, retorna NIL, indicando que não há solução;
2. Seleciona o primerio nó da lista de abertoss;
3. Se o estado do nó atual for objetivo, retorna o caminho até a esse nó;
4. Caso o nó não seja objetivo, gera os sucessores do nó atual, aplicando a função heurística;
5. Remove os nós com maior custo das listas de abertos e fechados em comparação com os sucessores gerados;
6. Remove os sucessores que já estão em abertos ou fechados;
7. Insere os sucessores válidos na lista de abertos filtrada;
8. Realiza a chamada recursvia com as novas listas de abertos e fechados.

## Descrição das opções tomadas

Todas as decisões tiveram por base os conceitos aprendidos durante a UC de IA, nomeadamente a utilização do paradigma funcional, a abordagem recursiva e a
não utilização de funções destrutivas.

O paradigma funcional permite que a solução tenha separação de interesses e que o código seja de mais fácil manutenção.

A abordagem recursiva permite desenvolver soluções mais concisas para problemas complexos, melhorando a legibilidade.

Ao usar o paradigma funcional, é favorecida a imutabilidade dos dados.

## Limitações técnicas e ideias para desenvolvimento futuro

Existem algumas limitações técnias no funcionamento do presente projeto, nomeadamente as limitações de memória do IDE LispWorks.

Desta forma, não é possível encontrar solução para os problemas D, F e G descritos no enunciado. Isto deve-se ao facto destes tabuleiros possuirem uma grande
quantidade de peças, logo, ao gerar sucessores, existe uma explosão combinatória.

Algumas ideias para desenvolvimento futuro são:

- Implementação de algoritmos de procura em espaço de estados que usem procura com memória limitado, como o IDA*, RBFS e SMA*;
- Permitir ao utilizador adicionar problemas;
- Gerar problemas aleatórios;
- Permitir a comparação entre algoritmos.

## Comparação dos Algoritmos

## Conclusão

Ao longo deste projeto, foi possível aplicar na prática os conhecimentos teóricos adquiridos na UC de IA, nomeadamente no que diz respeito aos algoritmos de
procura em espaços de estados.

A implementação da solução para o problema Adji-boto em LISP proporcionou uma valiosa experiência de programação e um aprofundamento da compreensão dos conceitos
fundamentais de IA.
O desenvolvimento deste programa não só permitiu consolidar os conhecimentos sobre os algoritmos de procura, mas também possibilitou a melhoria das habilidades de
resolução de problemas e de pensamento lógico.

Além disso, este projeto evidenciou a importância da eficiência computacional na implementação de soluções para problemas complexos.
Os alunos foram confrontados com a necessidade de otimizar seus algoritmos para lidar com o espaço de estados potencialmente extenso do Adji-boto, o que resultou numa
compreensão mais profunda das implicações práticas das escolhas algorítmicas.

Por fim, a realização deste trabalho contribuiu significativamente para o desenvolvimento de competências essenciais para futuros engenheiros informáticos, tais como a
capacidade de análise, design de soluções, implementação de código e documentação de projetos.
Estas habilidades serão certamente valiosas tanto no restante do percurso académico como na futura vida profissional dos alunos.
