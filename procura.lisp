;;;; procura.lisp
;;;; Alogritmos de porcura
;;;; Autores: Rodrigo Santos e Jo�o Fernandes

;; Travessias
(defun bfs (estado &optional (abertos (list (cria-no estado))) (fechados '()))
  "Pesquisa em largura funcional puramente recursiva"
  (if (null abertos) 
      nil  ; Caso a lista de abertos esteja vazia, ent�o n�o encontrou o objetivo
      (let* ((node (first abertos))  ; Primeiro n� (n� a ser processado)
             (abertos-sem-node (rest abertos))  ; Remove o n� atual dos abertos
             (novo-fechados (cons node fechados))  ; Adiciona o n� atual aos fechados
             (sucessores-validos (mapcar
                                (lambda (sucessor)
                                  (if (and (not (lista-tem-no sucessor abertos)) 
                                           (not (lista-tem-no sucessor fechados)))
                                      sucessor))  ; Retorna o sucessor se for v�lido
                                (sucessores node (gerar-operadores (first node)))))  ; Aplica o filtro a todos os sucessores
             (novo-abertos (append abertos-sem-node sucessores-validos))  ; Remove os nil e adiciona aos abertos
             (no-objetivo (filtrar-nos-objetivos sucessores-validos)))  ; Filtra os n�s objetivo
(if (not (null no-objetivo))  ; Verifica se � o objetivo
            (caminho no-objetivo novo-fechados)  ; Retorna o caminho
            (bfs nil novo-abertos novo-fechados)))))  ; Chamada recursiva

(defun dfs (estado &optional (limite most-positive-fixnum) (abertos (list (cria-no estado 0))) (fechados '()))
  "Pesquisa em profundidade limitada com verifica��o de custo."
  (if (null abertos)
      nil  ; Caso a lista de abertos esteja vazia, n�o encontrou o objetivo
      (let* ((node (first abertos))  ; Primeiro n� (n� a ser processado)
             (profundidade (second node))  ; Profundidade do n� atual
             (abertos-sem-node (rest abertos))  ; Remove o n� atual dos abertos
             (novo-fechados (cons node fechados)))  ; Adiciona o n� atual aos fechados
        (if (>= profundidade limite)
            ;; Se atingiu o limite, continua para o pr�ximo n�
            (dfs estado limite abertos-sem-node novo-fechados)
            (let* ((sucessores-validos (mapcar
                     (lambda (sucessor)
                       (if (not (lista-tem-no sucessor abertos))
                           sucessor))
                     (sucessores node (gerar-operadores (first node))))) ; Aplica o filtro a todos os sucessores
                   (fechados-atualizado (substituir-nodes-com-menor-custo novo-fechados sucessores-validos))
                   (sucessores-filtrados (mapcar
                       (lambda (sucessor)
                         (if (not (lista-tem-no sucessor fechados-atualizado))
                             sucessor))
                       sucessores-validos))
                   (novos-abertos (append sucessores-filtrados abertos-sem-node))  ; Remove sucessores inv�lidos
                   (no-objetivo (filtrar-nos-objetivos sucessores-filtrados)))  ; Filtra os n�s objetivo
              (if (not (null no-objetivo))
                  (caminho no-objetivo fechados-atualizado)  ; Retorna o caminho se encontrou o objetivo
                  (dfs estado limite novos-abertos fechados-atualizado)))))))


(defun a-star (estado fHeuristica &optional (abertos (list (cria-no estado))) (fechados '()))
  "Pesquisa em largura funcional puramente recursiva"
  (if (null abertos) nil  ; Caso a lista de abertos esteja vazia, ent�o n�o encontrou o objetivo
      (let* ((node (first abertos))  ; Primeiro n� (n� a ser processado)
             (novo-abertos (rest abertos))  ; Remove o n� atual dos abertos
             (novo-fechados (cons node fechados)))  ; Adiciona o n� atual aos fechados
             (if (tabuleiro-vaziop (first node)) (caminho node novo-fechados)
             (let* ((sucessores-gerados (sucessores node (gerar-operadores (first node)) fHeuristica)) ; Gera sucessores
                   (abertos-filtrados (remover-nodes-com-maior-custo novo-abertos sucessores-gerados))
                   (fechados-filtrados (remover-nodes-com-maior-custo novo-fechados sucessores-gerados))
                   (sucessores-validos (mapcar
                     (lambda (sucessor)
                       (if (and (not (lista-tem-no sucessor abertos-filtrados)) (not (lista-tem-no sucessor fechados-filtrados)))
                           sucessor))
                     sucessores-gerados)) 
                   (novos-abertos (inserir-nodes abertos-filtrados sucessores-validos)))  ; Adiciona novos sucessores aos abertos
                   (a-star nil fHeuristica novos-abertos fechados-filtrados))))))

(defun filter-sucessores (sucessores abertos fechados)
  "Filtra sucessores que n�o est�o em abertos ou fechados"
  (if (null sucessores)
      nil
      (let* ((s (first sucessores))
             (rest-sucessores (rest sucessores)))
        (if (or (member (third s) (mapcar #'third abertos) :test #'equal)
                (member (third s) (mapcar #'third fechados) :test #'equal))
            (filter-sucessores rest-sucessores abertos fechados)  ; Ignora o sucessor
            (cons s (filter-sucessores rest-sucessores abertos fechados))))))  ; Inclui o sucessor que n�o foi visitado

(defun lista-tem-no (node lista)
  (member (first node) (mapcar #'first lista) :test #'equal)
)

(defun filtrar-nos-objetivos (sucessores)
  "Retorna o primeiro n� objetivo (tabuleiro vazio) encontrado na lista de sucessores."
  (if (null sucessores)
      nil  ; Caso base: lista vazia, retorna nil
      (let* ((node (first sucessores))  ; Pega o primeiro sucessor
             (rest-sucessores (rest sucessores)))  ; Restante da lista
        (if (tabuleiro-vaziop (first node))  ; Verifica se � o n� objetivo
            node  ; Retorna o n� objetivo imediatamente
            (filtrar-nos-objetivos rest-sucessores)))))  ; Continua a busca nos sucessores restantes

(defun inserir-nodes (novos-nodes lista)
  "Insere todos os nodes da lista 'novos-nodes' na lista 'lista', mantendo a ordem por custo."
  (if (null novos-nodes)  ; Caso base: se n�o houver novos nodes para processar
      lista               ; Retorna a lista original
      (let ((node (first novos-nodes))  ; Pega o primeiro node de novos-nodes
            (custo (fourth (first novos-nodes)))) ; Custo do node atual
        (if (or (null lista)  ; Se a lista estiver vazia
                (< custo (fourth (first lista)))) ; Ou se o custo do novo node for menor que o do primeiro elemento da lista
            (cons node (inserir-nodes (rest novos-nodes) lista)) ; Insere o node no in�cio
            (cons (first lista) ; Mant�m o primeiro elemento da lista
                  (inserir-nodes novos-nodes (rest lista))))))) ; Continua com o resto da lista


(defun substituir-nodes-com-menor-custo (lista novos-elementos)
  "Substitui os elementos na lista original por novos elementos se o estado for o mesmo e o custo for menor."
  (mapcar (lambda (elemento)
            (let ((estado-original (first elemento))  ; Estado do elemento original
                  (custo-original (fourth elemento)))  ; Custo do elemento original
              (let ((melhor-elemento
                     (find estado-original novos-elementos :key #'first :test #'equal)))  ; Encontra o novo elemento com o mesmo estado
                (if (and melhor-elemento  ; Se encontrou um novo elemento com o mesmo estado
                         (< (fourth melhor-elemento) custo-original))  ; E o custo do novo elemento � menor
                    melhor-elemento  ; Substitui pelo novo elemento
                    elemento))))  ; Caso contr�rio, mant�m o elemento original
          lista))

(defun remover-nodes-com-maior-custo (lista novos-elementos)
  "Retorna uma nova lista contendo apenas os elementos que n�o t�m custo maior do que elementos equivalentes em novos-elementos."
  (if (null lista)
      nil
      (let* ((elemento (car lista))
             (estado-original (first elemento))
             (custo-original (fourth elemento))
             (melhor-elemento (find estado-original novos-elementos
                                    :key #'first
                                    :test #'equal)))
        (if (or (not melhor-elemento)
                (>= (fourth melhor-elemento) custo-original))
            (cons elemento 
                  (remover-nodes-com-maior-custo (cdr lista) novos-elementos))
            (remover-nodes-com-maior-custo (cdr lista) novos-elementos)))))


(defun caminho (node fechados &optional (solucao '()))
  (let* ((node-pai (third node))
         (estado (first node)))
    (if (null node-pai)
        (cons node solucao)
        (caminho node-pai
                fechados
                (cons node solucao)))))

;; Gera��o de sucessores
(defun cria-no (estado &optional (custo 0) (pai nil) (heuristica 0))
  (list estado custo pai (+ heuristica custo))
)

(defun novo-sucessor (no operador &optional (heuristica nil))
  (cria-no (funcall operador (first no)) (1+ (second no)) no (if (null heuristica) 0 (funcall heuristica no)))
)

(defun sucessores (no operadores &optional (heuristica nil))
  (mapcar (lambda (op) (novo-sucessor no op heuristica)) operadores)
)

(defun substituir-no (lista novoNode)
  "Substitui um n� na lista se o estado for igual e a heur�stica do novo n� for menor."
  (let* ((novo-estado (first novoNode))       ;; O estado do novoNode
         (nova-heuristica (fourth novoNode))) ;; A heur�stica do novoNode
    (mapcar (lambda (no)
              (if (and (equal (first no) novo-estado)  ;; Estados iguais
                       (> (fourth no) nova-heuristica)) ;; Heur�stica maior
                  novoNode  ;; Substitui o n� pelo novoNode
                  no))      ;; Caso contr�rio, mant�m o n� original
            lista)))

(defun heuristica-teste (elemento)
 1
)