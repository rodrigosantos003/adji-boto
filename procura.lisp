;;;; procura.lisp
;;;; Alogritmos de porcura
;;;; Autores: Rodrigo Santos e João Fernandes

;; Travessias
(defun bfs (estado &optional (abertos (list (cria-no estado))) (fechados '()))
  "Pesquisa em largura funcional puramente recursiva"
  (if (null abertos) nil  ; Caso a lista de abertos esteja vazia, então não encontrou o objetivo
      (let* ((node (first abertos))  ; Primeiro nó (nó a ser processado)
             (novo-abertos (rest abertos))  ; Remove o nó atual dos abertos
             (novo-fechados (cons node fechados)))  ; Adiciona o nó atual aos fechados
            (let* ((sucessores-gerados (sucessores node (gerar-operadores (first node))))  ; Gera sucessores
                   (filtrados (filter-sucessores sucessores-gerados novo-abertos novo-fechados))  ; Filtra sucessores não visitados
                   (novos-abertos (append novo-abertos filtrados))  ; Adiciona novos sucessores aos abertos
                   (no-objetivo (filtrar-nos-objetivos sucessores-gerados)))
              (if (not (null no-objetivo))  ; Verifica se é o objetivo
                  (caminho no-objetivo novo-fechados)  ; Retorna o caminho
              (bfs nil novos-abertos novo-fechados))))))  ; Chamada recursiva

(defun dfs (estado &optional (limite most-positive-fixnum) (abertos (list (cria-no estado 0))) (fechados '()))
  "Pesquisa em profundidade limitada funcional puramente recursiva."
  (if (null abertos) 
      nil  ; Caso a lista de abertos esteja vazia, não encontrou o objetivo
      (let* ((node (first abertos))  ; Primeiro nó (nó a ser processado)
             (profundidade (second node))  ; Profundidade do nó atual
             (novo-abertos (rest abertos))  ; Remove o nó atual dos abertos
             (novo-fechados (cons node fechados)))  ; Adiciona o nó atual aos fechados
        (if (>= profundidade limite) 
            (dfs nil limite novo-abertos novo-fechados)  ; Se atingiu o limite, continua para o próximo nó
            (let* ((sucessores-gerados 
                     (sucessores node (gerar-operadores (first node))))  ; Gera sucessores
                   (filtrados 
                     (filter-sucessores sucessores-gerados novo-abertos novo-fechados))  ; Filtra sucessores não visitados
                   (novos-abertos 
                     (append filtrados novo-abertos))  ; Adiciona novos sucessores no início dos abertos (ordem LIFO)
                   (no-objetivo 
                     (filtrar-nos-objetivos sucessores-gerados)))  ; Verifica se algum é objetivo
              (if (not (null no-objetivo)) 
                  (caminho no-objetivo novo-fechados)  ; Retorna o caminho se encontrou o objetivo
                  (dfs nil limite novos-abertos novo-fechados)))))))  ; Chamada recursiva

(defun filter-sucessores (sucessores abertos fechados)
  "Filtra sucessores que não estão em abertos ou fechados"
  (if (null sucessores)
      nil
      (let* ((s (first sucessores))
             (rest-sucessores (rest sucessores)))
        (if (or (member (third s) (mapcar #'third abertos) :test #'equal)
                (member (third s) (mapcar #'third fechados) :test #'equal))
            (filter-sucessores rest-sucessores abertos fechados)  ; Ignora o sucessor
            (cons s (filter-sucessores rest-sucessores abertos fechados))))))  ; Inclui o sucessor que não foi visitado

(defun filtrar-nos-objetivos (sucessores)
  "Retorna o primeiro nó objetivo (tabuleiro vazio) encontrado na lista de sucessores."
  (if (null sucessores)
      nil  ; Caso base: lista vazia, retorna nil
      (let* ((node (first sucessores))  ; Pega o primeiro sucessor
             (rest-sucessores (rest sucessores)))  ; Restante da lista
        (if (tabuleiro-vaziop (first node))  ; Verifica se é o nó objetivo
            node  ; Retorna o nó objetivo imediatamente
            (filtrar-nos-objetivos rest-sucessores)))))  ; Continua a busca nos sucessores restantes


(defun caminho (node fechados &optional (solucao '()))
  (let* ((node-pai (third node))
         (estado (first node)))
    (if (null node-pai)
        (cons estado solucao)
        (caminho node-pai
                fechados
                (cons estado solucao)))))

;; Geração de sucessores
(defun cria-no (estado &optional (custo 0) (pai nil))
  (list estado custo pai)
)

(defun novo-sucessor (no operador)
  (cria-no (funcall operador (first no)) (1+ (second no)) no)
)

(defun sucessores (no operadores)
  (mapcar (lambda (op) (novo-sucessor no op)) operadores)
)
