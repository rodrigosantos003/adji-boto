;;;; procura.lisp
;;;; Alogritmos de porcura
;;;; Autores: Rodrigo Santos e João Fernandes

;; Travessias
(defun bfs (estadoInicial &optional (abertos (list (cria-no estadoInicial))) (fechados '()))
  (cond
    ((null abertos) nil)  ; Se a lista de abertos estiver vazia, retorna nil
    (t
     (let* ((no (first abertos))
            (abertos-sem-no (rest abertos))
            (sucessores-validos 
             (remove-if-not 
              (lambda (sucessor)
                (and (not (lista-tem-no sucessor abertos))
                     (not (lista-tem-no sucessor fechados))))
              (sucessores no (gerar-operadores (estado no))))))
       (cond
         ((not (null (filtrar-nos-objetivos sucessores-validos)))
          (caminho (filtrar-nos-objetivos sucessores-validos) novo-fechados))  ; Se há objetivos, retorna o caminho
         (t
          (bfs nil (append abertos-sem-no sucessores-validos) (cons no fechados))))))))  ; Caso contrário, chama recursivamente

(defun dfs (estadoInicial &optional (limite most-positive-fixnum) (abertos (list (cria-no estadoInicial 0))) (fechados '()))
  (resetar-contar-nos)
  (if (null abertos)
      nil  ; Retorna nil se não encontrar solução
      (let* ((no (first abertos))
             (profundidade (nivel no))
             (abertos-sem-no (rest abertos)))
        (cond
          ((>= profundidade limite) ; Se a profundidade exceder o limite, não continua
           (dfs nil limite abertos-sem-no novo-fechados))  ; Continua com o restante da lista de abertos
          (t ; Caso contrário, explora os sucessores
           (let* ((sucessores-validos
                   (remove-if-not 
                    (lambda (sucessor)
                      (and (not (lista-tem-no sucessor abertos))
                           (not (lista-tem-no sucessor novo-fechados))))
                    (sucessores no (gerar-operadores (estado no))))))
             (if (not (null (filtrar-nos-objetivos sucessores-validos)))
                 (caminho (filtrar-nos-objetivos sucessores-validos) novo-fechados)  ; Se houver objetivo, retorna o caminho
                 (dfs nil limite (append sucessores-validos abertos-sem-no) (cons no fechados)))))))))



(defun a-star (estadoInicial fHeuristica &optional (abertos (list (cria-no estadoInicial))) (fechados '()))
  (resetar-contar-nos)
  (if (null abertos)
      nil
      (let* ((no (first abertos))
             (novo-abertos (rest abertos))
             (novo-fechados (cons no fechados)))
        (if (tabuleiro-vaziop (estado no))
            (caminho no novo-fechados)
            (let* ((sucessores-gerados (sucessores no (gerar-operadores (estado no)) fHeuristica))
                   (abertos-filtrados (remover-nodes-com-maior-custo novo-abertos sucessores-gerados))
                   (fechados-filtrados (remover-nodes-com-maior-custo novo-fechados sucessores-gerados))
                   (sucessores-validos 
                    (remove-if 
                     (lambda (sucessor)
                       (or (lista-tem-no sucessor abertos)
                           (lista-tem-no sucessor fechados)))
                     sucessores-gerados))
                   (novos-abertos (inserir-nodes sucessores-validos abertos-filtrados)))
              (a-star nil fHeuristica novos-abertos fechados-filtrados))))))

;; Funções auxiliares da procura
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


(defun substituir-nodes-com-menor-custo (lista novos-elementos)
  (reduce 
   (lambda (acc elemento)
     (let* ((estado-original (estado elemento))
            (custo-original (custo elemento))
            (melhor-elemento (find estado-original novos-elementos :key #'estado :test #'equal)))
       (if (and melhor-elemento (< (custo melhor-elemento) custo-original))
           (cons melhor-elemento acc)
           (cons elemento acc))))
   lista
   :initial-value '()
   :from-end t))

(defun remover-nodes-com-maior-custo (lista novos-elementos)
  (remove-if 
   (lambda (elemento)
     (let* ((estado-original (estado elemento))
            (custo-original (custo elemento))
            (melhor-elemento (find estado-original novos-elementos :key #'estado :test #'equal)))
       (and melhor-elemento (< (custo melhor-elemento) custo-original))))
   lista))


(defun caminho (no fechados &optional (solucao '()))
  (let* ((no-pai (pai no))
         (no-estado (estado no)))
    (if (null no-pai)
        (cons no-estado solucao)
        (caminho no-pai
                fechados
                (cons no-estado solucao)))))

(defun estado (no) (first no))

(defun nivel (no) (second no))

(defun pai (no) (third no))

(defun custo (no) (fourth no))

;; Geração de sucessores
(defun cria-no (estadoNo &optional (nivelNo 0) (paiNo nil) (custoNo 0))
  (list estadoNo nivelNo paiNo custoNo))

(defun novo-sucessor (no operador &optional (heuristica nil))
  (let* ((novo-estado (funcall operador (estado no)))
         (novo-nivel (1+ (nivel no)))
         (novo-no (cria-no novo-estado novo-nivel no)))
    ;; Incrementa o contador de nós gerados
    (incrementar-nos)
    (if (null heuristica)
        novo-no
        (let ((custo-total (+ novo-nivel (funcall heuristica novo-no))))
          ;; Retorna uma nova versão do nó com o custo atualizado
          (incrementar-nos)  ;; Incrementa novamente ao criar o novo nó
          (cria-no (estado novo-no) novo-nivel (pai novo-no) custo-total))))
)


(defun sucessores (no operadores &optional (heuristica nil))
  (mapcar (lambda (op) (novo-sucessor no op heuristica)) operadores)
)

(defun substituir-no (lista novoNo)
  "Substitui um nó na lista se o estado for igual e a heurística do novo nó for menor."
  (let* ((novo-estado (estado novoNo))       ;; O estado do novoNo
         (nova-heuristica (custo novoNo))) ;; A heurística do novoNo
    (mapcar (lambda (no)
              (if (and (equal (estado no) novo-estado)  ;; Estados iguais
                       (> (custo no) nova-heuristica)) ;; Heurística maior
                  novoNo  ;; Substitui o nó pelo novoNode
                  no))      ;; Caso contrário, mantém o nó original
            lista)))

(defun raiz (no)
  (if (null (pai no))   ; Se o nó atual não tem pai, é a raiz
      no
      (raiz (pai no)))) ; Caso contrário, suba para o pai
