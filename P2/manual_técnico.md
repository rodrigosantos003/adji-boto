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

1. [Introdução](#1-introdução)
2. [Algoritmo](#2-algoritmo)
3. [Tipos Abstratos](#3-tipos-abstratos)
4. [Limitações e Opções Técnicas](#4-limitações-e-opções-técnicas)
5. [Resultados](#5-resultados)
6. [Análise Estatística](#6-análise-estatística)
7. [Conclusão](#7-conclusão)

## 1. Introdução

No âmbito do primeiro projeto da Unidade Curricular (UC) de Inteligência Artificial (IA), do 1º semestre do 3º ano da Licenciatura em Engenharia Informática,
foi solicitado aos alunos que implementassem um programa para resolução do problema Adji-boto, recorrendo um algoritmo de teoria de jogos.

O presente manual técnico tem como objetivo descrever a solução desenvolvida pelos alunos, bem como os detalhes da sua implementação.

## 2. Algoritmo

De forma a resolver os problemas, os alunos implementaram o algoritmo Negamax, usando uma abordagem funcional, recursiva e não-destrutiva.

```lisp
(defun negamax (node depth jogador &optional (alpha most-negative-fixnum) (beta most-positive-fixnum) (color 1))
  (if (or (zerop depth) (terminalp node))
      (* color (evaluate node jogador))
      (negamax-recursivo node depth jogador alpha beta color (or (remove nil (sucessores node jogador)) (list node)))))

(defun negamax-recursivo (node depth jogador alpha beta color children)
  (if (null children)
      alpha
      (let* ((child (car children))
             (score (- (negamax child (1- depth) (alternar-jogador jogador) (- beta) (- alpha) (- color)))))
        (incrementar-nos)
        (let ((new-alpha (max alpha score)))
          (if (>= new-alpha beta)
              (progn
                (incrementar-cortes)
                beta)
              (negamax-recursivo node depth jogador new-alpha beta color (cdr children)))))))
```

## 3. Tipos Abstratos

## 4. Limitações e Opções Técnicas

## 5. Resultados

## 6. Análise Estatística

## 7. Conclusão

Ao longo deste projeto, foi possível aplicar na prática os conhecimentos teóricos adquiridos na UC de IA, no que diz respeito aos algoritmos de
procura em espaços de estados.

A implementação da solução para o problema Adji-boto em LISP proporcionou uma valiosa experiência de programação e um aprofundamento da compreensão dos conceitos
fundamentais de IA.
O desenvolvimento deste programa não só permitiu consolidar os conhecimentos sobre os algoritmos de procura, mas também possibilitou a melhoria das competências de
resolução de problemas e de pensamento lógico.

Além disso, este projeto evidenciou a importância da eficiência computacional na implementação de soluções para problemas complexos.
Os alunos foram confrontados com a necessidade de otimizar os seus algoritmos para lidar com a explosão combinatória dos estados do problema Adji-boto, o que resultou
numa compreensão mais profunda das implicações práticas das escolhas algorítmicas.

Em suma, os alunos implementaram com sucesso uma solução eficaz para o problema proposto, aplicando os algoritmos em procura de espaços de estados lecionados
na UC de IA.
