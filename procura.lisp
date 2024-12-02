;;;; procura.lisp
;;;; Alogritmos de porcura
;;;; Autores: Rodrigo Santos e Jo�o Fernandes

;; Travessias
(defun bfs (tabuleiro &optional (abertos (list (list nil nil tabuleiro 0))) (fechados '()))
  "Pesquisa em largura funcional puramente recursiva"
  (if (null abertos) nil  ; Caso a lista de abertos esteja vazia, ent�o n�o encontrou o objetivo
      (let* ((node (first abertos))  ; Primeiro n� (n� a ser processado)
             (novo-abertos (rest abertos))  ; Remove o n� atual dos abertos
             (novo-fechados (cons node fechados)))  ; Adiciona o n� atual aos fechados
            (let* ((sucessores (gerar-filhos node))  ; Gera sucessores
                   (filtrados (filter-sucessores sucessores novo-abertos novo-fechados))  ; Filtra sucessores n�o visitados
                   (novos-abertos (append novo-abertos filtrados))  ; Adiciona novos sucessores aos abertos
                   (no-objetivo (filtrar-nos-objetivos sucessores)))
              (if (not (null no-objetivo))  ; Verifica se � o objetivo
                  (caminho no-objetivo novo-fechados)  ; Retorna o caminho
              (bfs nil novos-abertos novo-fechados))))))  ; Chamada recursiva

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

(defun filtrar-nos-objetivos (sucessores)
  "Retorna o primeiro n� objetivo (tabuleiro vazio) encontrado na lista de sucessores."
  (if (null sucessores)
      nil  ; Caso base: lista vazia, retorna nil
      (let* ((n� (first sucessores))  ; Pega o primeiro sucessor
             (rest-sucessores (rest sucessores)))  ; Restante da lista
        (if (tabuleiro-vaziop (third n�))  ; Verifica se � o n� objetivo
            n�  ; Retorna o n� objetivo imediatamente
            (filtrar-nos-objetivos rest-sucessores)))))  ; Continua a busca nos sucessores restantes


(defun caminho (node fechados &optional (solucao '()))
  (let* ((node-pai (first node))  ; Pega o tabuleiro-pai do n� completo
         (operacao (second node)))  ; Pega a opera��o do n� completo
    (if (null node-pai)  ; Se o tabuleiro pai � nil, retornamos o caminho
        solucao   ; O caminho deve ser invertido, pois estamos acumulando as opera��es em ordem reversa
        (caminho node-pai  ; Chama recursivamente com o n� pai
                 fechados
                 (cons operacao solucao)))))  ; Adiciona a opera��o ao caminho

;; Gera��o de sucessores
(defun gerar-filhos (node &optional (linha 0) (coluna 0) (resultados '()) (fHeuristica))
  "Gera sucessores de forma recursiva a partir de uma matriz, ignorando c�lulas n�o distribu�veis."
  (let ((matriz (third node)))
    (if (>= linha (length matriz)) ; Se percorremos todas as linhas, terminar.
        resultados
      (let ((nova-linha (if (>= coluna (length (nth linha matriz)))
                            (1+ linha)
                          linha))
            (nova-coluna (if (>= coluna (length (nth linha matriz)))
                             0
                           (1+ coluna))))
        (if (and (< linha (length matriz)) ; Validar que ainda estamos na matriz.
                 (< coluna (length (nth linha matriz)))
                 (celula-distribuivelp linha coluna matriz))
            (let* ((tabuleiro-pai node)
                   (operacao (list linha coluna))
                   (filho (operador linha coluna matriz))
                   (custo (+ (fourth node) 1))
                   (heuristica (if fHeuristica
                                   (funcall fHeuristica filho)
                                 0))
                   (resultado (list tabuleiro-pai operacao filho custo heuristica)))
              (gerar-filhos node nova-linha nova-coluna (cons resultado resultados)))
          (gerar-filhos node nova-linha nova-coluna resultados)))))) ; Avan�ar para a pr�xima c�lula.

(defun celula-distribuivelp (linha coluna matriz)
  "Retorna T se a c�lula tem pe�as suficientes para uma distribui��o v�lida."
  (let ((valor (celula linha coluna matriz)))
    (and (> valor 0) t)))


