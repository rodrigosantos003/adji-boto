;;;; procura.lisp
;;;; Alogritmos de porcura
;;;; Autores: Rodrigo Santos e João Fernandes

;; Travessias
(defun bfs (estado &optional (abertos (list (cria-no estado))) (fechados '()))
  "Pesquisa em largura funcional puramente recursiva"
  (if (null abertos) 
      nil  ; Caso a lista de abertos esteja vazia, então não encontrou o objetivo
      (let* ((node (first abertos))  ; Primeiro nó (nó a ser processado)
             (abertos-sem-node (rest abertos))  ; Remove o nó atual dos abertos
             (novo-fechados (cons node fechados))  ; Adiciona o nó atual aos fechados
             (sucessores-validos (mapcar
                                (lambda (sucessor)
                                  (if (and (not (lista-tem-no sucessor abertos)) 
                                           (not (lista-tem-no sucessor fechados)))
                                      sucessor))  ; Retorna o sucessor se for válido
                                (sucessores node (gerar-operadores (first node)))))  ; Aplica o filtro a todos os sucessores
             (novo-abertos (append abertos-sem-node sucessores-validos))  ; Remove os nil e adiciona aos abertos
             (no-objetivo (filtrar-nos-objetivos sucessores-validos)))  ; Filtra os nós objetivo
(if (not (null no-objetivo))  ; Verifica se é o objetivo
            (caminho no-objetivo novo-fechados)  ; Retorna o caminho
            (bfs nil novo-abertos novo-fechados)))))  ; Chamada recursiva

(defun dfs (estado &optional (limite most-positive-fixnum) (abertos (list (cria-no estado 0))) (fechados '()))
  "Pesquisa em profundidade limitada com verificação de custo."
  (if (null abertos)
      nil  ; Caso a lista de abertos esteja vazia, não encontrou o objetivo
      (let* ((node (first abertos))  ; Primeiro nó (nó a ser processado)
             (profundidade (second node))  ; Profundidade do nó atual
             (abertos-sem-node (rest abertos))  ; Remove o nó atual dos abertos
             (novo-fechados (cons node fechados)))  ; Adiciona o nó atual aos fechados
        (if (>= profundidade limite)
            ;; Se atingiu o limite, continua para o próximo nó
            (dfs estado limite abertos-sem-node novo-fechados)
            (let* ((sucessores-validos (mapcar
                                        (lambda (sucessor)
                                          (if (not (lista-tem-no sucessor abertos))
                                              sucessor)  ; Retorna o sucessor se for válido
                                        (sucessores node (gerar-operadores (first node))))))  ; Aplica o filtro a todos os sucessores
                   (novos-abertos (append abertos-sem-node sucessores-validos))  ; Remove sucessores inválidos
                   (no-objetivo (filtrar-nos-objetivos sucessores-validos)))  ; Filtra os nós objetivo
              (if (not (null no-objetivo))
                  (caminho no-objetivo novo-fechados)  ; Retorna o caminho se encontrou o objetivo
                  (dfs estado limite novos-abertos novo-fechados)))))))






(defun a-star (estado fHeuristica &optional (abertos (list (cria-no estado))) (fechados '()))
  "Pesquisa em largura funcional puramente recursiva"
  (if (null abertos) nil  ; Caso a lista de abertos esteja vazia, então não encontrou o objetivo
      (let* ((node (first abertos))  ; Primeiro nó (nó a ser processado)
             (novo-abertos (rest abertos))  ; Remove o nó atual dos abertos
             (novo-fechados (cons node fechados)))  ; Adiciona o nó atual aos fechados
             (if (tabuleiro-vaziop (first node)) (caminho node novo-fechados)
             (let* ((sucessores-gerados (sucessores node (gerar-operadores (first node)) fHeuristica))  ; Gera sucessores
                   (filtrados (filter-sucessores sucessores-gerados novo-abertos novo-fechados))  ; Filtra sucessores não visitados
                   (novos-abertos (append novo-abertos filtrados))  ; Adiciona novos sucessores aos abertos
                   (no-objetivo (filtrar-nos-objetivos sucessores-gerados)))
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

(defun lista-tem-no (node lista)
  (member (first node) (mapcar #'first lista) :test #'equal)
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

(defun inserir-node (node lista)
  "Insere um node na lista ordenada por custo (segundo campo do node)."
  (let ((custo (fourth node))) ; Obtem o segundo campo do node (custo)
    (if (or (null lista) ; Se a lista for vazia
            (< custo (fourth (first lista)))) ; Ou se o custo for menor que o custo do primeiro elemento
        (cons node lista) ; Insere o node no início da lista
        (cons (first lista) ; Mantém o primeiro elemento e processa o resto da lista
              (inserir-node node (rest lista))))))

(defun substituir-nodes-com-menor-custo (lista novos-elementos)
  "Substitui os elementos na lista original por novos elementos se o estado for o mesmo e o custo for menor."
  (mapcar (lambda (elemento)
            (let ((estado-original (first elemento))  ; Estado do elemento original
                  (custo-original (second elemento)))  ; Custo do elemento original
              (let ((melhor-elemento
                     (find estado-original novos-elementos :key #'first :test #'equal)))  ; Encontra o novo elemento com o mesmo estado
                (if (and melhor-elemento  ; Se encontrou um novo elemento com o mesmo estado
                         (< (second melhor-elemento) custo-original))  ; E o custo do novo elemento é menor
                    melhor-elemento  ; Substitui pelo novo elemento
                    elemento))))  ; Caso contrário, mantém o elemento original
          lista))



(defun caminho (node fechados &optional (solucao '()))
  (let* ((node-pai (third node))
         (estado (first node)))
    (if (null node-pai)
        (cons estado solucao)
        (caminho node-pai
                fechados
                (cons estado solucao)))))

;; Geração de sucessores
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
  "Substitui um nó na lista se o estado for igual e a heurística do novo nó for menor."
  (let* ((novo-estado (first novoNode))       ;; O estado do novoNode
         (nova-heuristica (fourth novoNode))) ;; A heurística do novoNode
    (mapcar (lambda (no)
              (if (and (equal (first no) novo-estado)  ;; Estados iguais
                       (> (fourth no) nova-heuristica)) ;; Heurística maior
                  novoNode  ;; Substitui o nó pelo novoNode
                  no))      ;; Caso contrário, mantém o nó original
            lista)))