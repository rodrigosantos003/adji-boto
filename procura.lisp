;;;; procura.lisp
;;;; Alogritmos de procura
;;;; Autores: Rodrigo Santos e João Fernandes

;; Travessias
(defun bfs (estadoInicial &optional (abertos (list (cria-no estadoInicial))) (fechados '()))
  (cond
    ((null abertos) nil)  ; Se a lista de abertos estiver vazia, retorna nil
    (t
     (let ((sucessores-validos
            (remove-if-not
             (lambda (sucessor)
               (and (not (lista-tem-no sucessor abertos))
                    (not (lista-tem-no sucessor fechados))))
             (sucessores (first abertos) (gerar-operadores (estado (first abertos)))))))
       (cond
         ((not (null (filtrar-nos-objetivos sucessores-validos)))
          (apresentar-resultado (filtrar-nos-objetivos sucessores-validos) (cons (first abertos) fechados)))
         (t
          (bfs estadoInicial
               (append (rest abertos) sucessores-validos)
               (cons (first abertos) fechados))))))))

(defun dfs (estadoInicial &optional (limite most-positive-fixnum) (abertos (list (cria-no estadoInicial 0))) (fechados '()))
  (if (null abertos)
      nil  ; Retorna nil se não encontrar solução
      (if (>= (nivel (first abertos)) limite)  ; Se a profundidade exceder o limite, não continua
          (dfs nil limite (rest abertos) (cons (first abertos) fechados))
          (let ((sucessores-validos
                 (remove-if-not 
                  (lambda (sucessor)
                    (and (not (lista-tem-no sucessor abertos))
                         (not (lista-tem-no sucessor fechados))))
                  (sucessores (first abertos) (gerar-operadores (estado (first abertos)))))))
            (if (not (null (filtrar-nos-objetivos sucessores-validos)))
                (apresentar-resultado (filtrar-nos-objetivos sucessores-validos) (cons (first abertos) fechados))  ; Se houver objetivo, retorna o caminho
                (dfs estadoInicial limite 
                     (append sucessores-validos (rest abertos))
                     (cons (first abertos) fechados)))))))

(defun a-star (estadoInicial fHeuristica &optional (abertos (list (cria-no estadoInicial))) (fechados '()))
  "Executa o algoritmo A* usando recursão em cauda otimizada."
  (if (null abertos)
      nil ; Caso a lista de abertos esteja vazia, falha.
      (let ((no-atual (first abertos)))
        (if (tabuleiro-vaziop (estado no-atual))
            (apresentar-resultado no-atual (cons no-atual fechados)) ; Se o tabuleiro estiver vazio, retorna o caminho.
            (let* ((sucessores-validos
                    (filtra-sucessores
                     (sucessores no-atual (gerar-operadores (estado no-atual)) fHeuristica)
                     (rest abertos)
                     fechados)))
              (a-star 
               ;; Estado
               nil 
               ;; Heuristica
               fHeuristica 
               ;; Abertos
               (inserir-nodes sucessores-validos (rest abertos)) 
               ;; Fechados
               (atualiza-fechados no-atual fechados sucessores-validos)))))))

(defun filtra-sucessores (sucessores abertos fechados)
  "Filtra os sucessores válidos."
  (remove-if-not
   (lambda (sucessor)
     (or (nao-existep sucessor abertos fechados)
         (melhor-custop sucessor abertos)
         (melhor-custop sucessor fechados)))
   sucessores))

;; Funções auxiliares da procura

(defun nao-existep (no lista1 lista2)
  "Verifica se o nó não existe em nenhuma das duas listas."
  (and (not (find no lista1 :key #'estado :test #'equal))
       (not (find no lista2 :key #'estado :test #'equal))))

(defun melhor-custop (no lista)
  "Verifica se o nó tem um custo menor que o nó correspondente na lista."
  (let ((no-existente (find no lista :key #'estado :test #'equal)))
    (and no-existente (< (custo no) (custo no-existente)))))

(defun atualiza-fechados (no-atual fechados sucessores-validos)
  "Atualiza a lista de fechados, removendo duplicatas."
  (cons no-atual
        (remove-if
         (lambda (no)
           (find no sucessores-validos :key #'estado :test #'equal))
         fechados)))

(defun lista-tem-no (node lista)
  (member (estado node) (mapcar #'estado lista) :test #'equal)
)

(defun filtrar-nos-objetivos (sucessores)
  "Retorna o primeiro nó objetivo (tabuleiro vazio) encontrado na lista de sucessores."
  (if (null sucessores)
      nil  ; Caso base: lista vazia, retorna nil
      (let* ((node (first sucessores))  ; Pega o primeiro sucessor
             (rest-sucessores (rest sucessores)))  ; Restante da lista
        (if (tabuleiro-vaziop (first node))  ; Verifica se é o nó objetivo
            node  ; Retorna o nó objetivo imediatamente
            (filtrar-nos-objetivos rest-sucessores)))))  ; Continua a busca nos sucessores restantes

(defun inserir-nodes (novos-nos lista)
  (reduce 
   (lambda (acc no)
     (let ((custoAtual (custo no)))
       (if (or (null acc) (< custoAtual (custo (first acc))))
           (cons no acc)
           (cons (first acc) (inserir-nodes (list no) (rest acc))))))
   novos-nos
   :initial-value lista))

(defun caminho (no fechados &optional (solucao '()))
(if (null (pai no))
        (cons no solucao)
        (caminho (pai no)
                fechados
                (cons no solucao))))

(defun apresentar-caminho (solucao)
  (if (null solucao)
      nil  ; Caso base: se a lista estiver vazia, termina a recursão
      (progn
        (format t "~A - ~A~%" (estado (first solucao)) (custo (first solucao)))  ; Imprime o estado e o custo
        (apresentar-caminho (rest solucao)))))  ; Chamada recursiva para o próximo nó

(defun apresentar-resultado (no fechados)
  (let ((solucao (caminho no fechados)))  ; Obtém o caminho
    (format t "RESULTADO~%")
    (format t "Estado inicial:  \"~A\"~%" (estado (first solucao)))  ; Imprime o estado inicial
    (format t "Caminho:~%")
    (apresentar-caminho solucao)
    (apresentar-desempenho solucao)))  ; Chama a função recursiva para imprimir o caminho


(defun estado (no) (first no))

(defun nivel (no) (second no))

(defun pai (no) (third no))

(defun custo (no) (fourth no))

;; Geração de sucessores
(defun cria-no (estadoNo &optional (nivelNo 0) (paiNo nil) (custoNo 0))
  (list estadoNo nivelNo paiNo (+ nivelNo custoNo)))

(defun novo-sucessor (no operador &optional (heuristica nil))
  (let* ((novo-estado (funcall operador (estado no)))
         (novo-nivel (1+ (nivel no)))
         (novo-no (cria-no novo-estado novo-nivel no)))
    ;; Incrementa o contador de nós gerados
    (incrementar-nos)
    (cond
      ((null heuristica) novo-no)  ;; Se heurística for nula, retorna o novo nó sem a heurística
      (t (incrementar-nos)  ;; Incrementa novamente ao criar o novo nó com heurística
          (cria-no (estado novo-no) novo-nivel (pai novo-no) (funcall heuristica novo-no))))))

(defun sucessores (no operadores &optional (heuristica nil))
  (mapcar (lambda (op) (novo-sucessor no op heuristica)) operadores)
)

(defun raiz (no)
  (if (null (pai no))   ; Se o nó atual não tem pai, é a raiz
      no
      (raiz (pai no)))) ; Caso contrário, suba para o pai
