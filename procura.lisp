;;;; procura.lisp
;;;; Alogritmos de porcura
;;;; Autores: Rodrigo Santos e João Fernandes

;; Travessias
(defun bfs (tabuleiro &optional (abertos (list (list nil nil tabuleiro 0))) (fechados '()))
  "Pesquisa em largura funcional puramente recursiva"
  (if (null abertos) nil  ; Caso a lista de abertos esteja vazia, então não encontrou o objetivo
      (let* ((node (first abertos))  ; Primeiro nó (nó a ser processado)
             (novo-abertos (rest abertos))  ; Remove o nó atual dos abertos
             (novo-fechados (cons node fechados)))  ; Adiciona o nó atual aos fechados
            (let* ((sucessores (gerar-filhos node))  ; Gera sucessores
                   (filtrados (filter-sucessores sucessores novo-abertos novo-fechados))  ; Filtra sucessores não visitados
                   (novos-abertos (append novo-abertos filtrados))  ; Adiciona novos sucessores aos abertos
                   (no-objetivo (filtrar-nos-objetivos sucessores)))
              (if (not (null no-objetivo))  ; Verifica se é o objetivo
                  (caminho no-objetivo novo-fechados)  ; Retorna o caminho
              (bfs nil novos-abertos novo-fechados))))))  ; Chamada recursiva

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
      (let* ((nó (first sucessores))  ; Pega o primeiro sucessor
             (rest-sucessores (rest sucessores)))  ; Restante da lista
        (if (tabuleiro-vaziop (third nó))  ; Verifica se é o nó objetivo
            nó  ; Retorna o nó objetivo imediatamente
            (filtrar-nos-objetivos rest-sucessores)))))  ; Continua a busca nos sucessores restantes


(defun caminho (node fechados &optional (solucao '()))
  (let* ((node-pai (first node))  ; Pega o tabuleiro-pai do nó completo
         (operacao (second node)))  ; Pega a operação do nó completo
    (if (null node-pai)  ; Se o tabuleiro pai é nil, retornamos o caminho
        solucao   ; O caminho deve ser invertido, pois estamos acumulando as operações em ordem reversa
        (caminho node-pai  ; Chama recursivamente com o nó pai
                 fechados
                 (cons operacao solucao)))))  ; Adiciona a operação ao caminho

;; Geração de sucessores
(defun cria-no (estado &optional (custo 0) (pai nil))
  (list estado custo pai)
)

(defun novo-sucessor (no operador)
  (cria-no (funcall operador (first no)) (second no) no)
)

(defun sucessores (no operadores)
  (mapcar (lambda (op) (novo-sucessor no op)) operadores)
)
