;;;; procura.lisp
;;;; Alogritmos de porcura
;;;; Autores: Rodrigo Santos e João Fernandes

;; Travessias
(defun bfs (estadoInicial &optional (abertos (list (cria-no estadoInicial))) (fechados '()))
  (resetar-contar-nos)
  "Pesquisa em largura funcional puramente recursiva"
  (if (null abertos) 
      nil  ; Caso a lista de abertos esteja vazia, então não encontrou o objetivo
      (let* ((no (first abertos))  ; Primeiro nó (nó a ser processado)
             (abertos-sem-no (rest abertos))  ; Remove o nó atual dos abertos
             (novo-fechados (cons no fechados))  ; Adiciona o nó atual aos fechados
             (sucessores-validos (mapcar
                                (lambda (sucessor)
                                  (if (and (not (lista-tem-no sucessor abertos)) 
                                           (not (lista-tem-no sucessor fechados)))
                                      sucessor))  ; Retorna o sucessor se for válido
                                (sucessores no (gerar-operadores (estado no)))))  ; Aplica o filtro a todos os sucessores
             (novo-abertos (append abertos-sem-no sucessores-validos))  ; Remove os nil e adiciona aos abertos
             (no-objetivo (filtrar-nos-objetivos sucessores-validos)))  ; Filtra os nós objetivo
(if (not (null no-objetivo))  ; Verifica se é o objetivo
            (caminho no-objetivo novo-fechados)  ; Retorna o caminho
            (bfs nil novo-abertos novo-fechados)))))  ; Chamada recursiva

(defun dfs (estadoInicial &optional (limite most-positive-fixnum) (abertos (list (cria-no estadoInicial 0))) (fechados '()))
  "Pesquisa em profundidade limitada com verificação de custo."
  (resetar-contar-nos)
  (if (null abertos)
      nil  ; Caso a lista de abertos esteja vazia, não encontrou o objetivo
      (let* ((no (first abertos))  ; Primeiro nó (nó a ser processado)
             (profundidade (nivel no))  ; Profundidade do nó atual
             (abertos-sem-no (rest abertos))  ; Remove o nó atual dos abertos
             (novo-fechados (cons no fechados)))  ; Adiciona o nó atual aos fechados
        (if (>= profundidade limite)
            ;; Se atingiu o limite, continua para o próximo nó
            (dfs nil limite abertos-sem-no novo-fechados)
            (let* ((sucessores-validos (mapcar
                     (lambda (sucessor)
                       (if (not (lista-tem-no sucessor abertos))
                           sucessor))
                     (sucessores no (gerar-operadores (estado no))))) ; Aplica o filtro a todos os sucessores
                   (fechados-atualizado (substituir-nodes-com-menor-custo novo-fechados sucessores-validos))
                   (sucessores-filtrados (mapcar
                       (lambda (sucessor)
                         (if (not (lista-tem-no sucessor fechados-atualizado))
                             sucessor))
                       sucessores-validos))
                   (novos-abertos (append sucessores-filtrados abertos-sem-no))  ; Remove sucessores inválidos
                   (no-objetivo (filtrar-nos-objetivos sucessores-filtrados)))  ; Filtra os nós objetivo
              (if (not (null no-objetivo))
                  (caminho no-objetivo fechados-atualizado)  ; Retorna o caminho se encontrou o objetivo
                  (dfs nil limite novos-abertos fechados-atualizado)))))))

(defun a-star (estadoInicial fHeuristica &optional (abertos (list (cria-no estadoInicial))) (fechados '()))
  "Pesquisa em largura funcional puramente recursiva"
  (resetar-contar-nos)
  (if (null abertos) nil  ; Caso a lista de abertos esteja vazia, então não encontrou o objetivo
      (let* ((no (first abertos))  ; Primeiro nó (nó a ser processado)
             (novo-abertos (rest abertos))  ; Remove o nó atual dos abertos
             (novo-fechados (cons no fechados)))  ; Adiciona o nó atual aos fechados
             (if (tabuleiro-vaziop (estado no)) (caminho no novo-fechados)
             (let* ((sucessores-gerados (sucessores no (gerar-operadores (estado no)) fHeuristica)) ; Gera sucessores
                   (abertos-filtrados (remover-nodes-com-maior-custo novo-abertos sucessores-gerados))
                   (fechados-filtrados (remover-nodes-com-maior-custo novo-fechados sucessores-gerados))
                   (sucessores-validos (reduce 
                                        (lambda (acc sucessor)
                                          (if (and (not (lista-tem-no sucessor abertos))
                                                   (not (lista-tem-no sucessor fechados)))
                                              (cons sucessor acc)
                                            acc))
                                        sucessores-gerados
                                        :initial-value '()))
                   (novos-abertos (inserir-nodes sucessores-validos abertos-filtrados)))  ; Adiciona novos sucessores aos abertos
                   (a-star nil fHeuristica novos-abertos fechados-filtrados))))))

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
  "Insere todos os nodes da lista 'novos-nodes' na lista 'lista', mantendo a ordem por custo."
  (if (null novos-nos)  ; Caso base: se não houver novos nodes para processar
      lista               ; Retorna a lista original
      (let ((no (first novos-nos))  ; Pega o primeiro node de novos-nodes
            (custoAtual (custo (first novos-nos)))) ; Custo do node atual
        (if (or (null lista)  ; Se a lista estiver vazia
                (< custoAtual (custo (first lista)))) ; Ou se o custo do novo node for menor que o do primeiro elemento da lista
            (cons no (inserir-nodes (rest novos-nos) lista)) ; Insere o node no início
            (cons (first lista) ; Mantém o primeiro elemento da lista
                  (inserir-nodes novos-nos (rest lista))))))) ; Continua com o resto da lista


(defun substituir-nodes-com-menor-custo (lista novos-elementos)
  "Substitui os elementos na lista original por novos elementos se o estado for o mesmo e o custo for menor."
  (mapcar (lambda (elemento)
            (let ((estado-original (first elemento))  ; Estado do elemento original
                  (custo-original (fourth elemento)))  ; Custo do elemento original
              (let ((melhor-elemento
                     (find estado-original novos-elementos :key #'first :test #'equal)))  ; Encontra o novo elemento com o mesmo estado
                (if (and melhor-elemento  ; Se encontrou um novo elemento com o mesmo estado
                         (< (fourth melhor-elemento) custo-original))  ; E o custo do novo elemento é menor
                    melhor-elemento  ; Substitui pelo novo elemento
                    elemento))))  ; Caso contrário, mantém o elemento original
          lista))

(defun remover-nodes-com-maior-custo (lista novos-elementos)
  "Retorna uma nova lista contendo apenas os elementos que não têm custo maior do que elementos equivalentes em novos-elementos."
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
