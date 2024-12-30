;;; Ficheiro: desempenho.lisp
;;; Fun��es para calcular as medidas de desempenho dos algoritmos
;;; Autores: Jo�o Fernandes e Rodrigo Santos

; Vari�vel global para contar o n�mero de n�s gerados
(defparameter *numero-nos-gerados* 0.0)

(defun incrementar-nos ()
  "Incrementa o contador de n�s gerados"
  (incf *numero-nos-gerados*))

(defun repor-contar-nos ()
  "Rep�e a contagem de n�s gerados"
  (setf *numero-nos-gerados* 0.0))

(defun apresentar-desempenho (c)
  "Apresenta o desempenho de um caminho"
  (format t "N�mero n�s: ~A ~%" *numero-nos-gerados*)
  (format t "Comprimento caminho: ~A ~%" (comprimento-caminho c))
  (format t "Penetr�ncia: ~,vF ~%" 2 (penetrancia c))
  (format t "Fator Ramifica��o: ~,vF ~%" 2 (float (fator-ramificacao (comprimento-caminho c) *numero-nos-gerados* 0 100 0.1)))
)

;; Penetr�ncia
(defun penetrancia (caminho)
  "Calcula a penetr�ncia de um algortimo (P = L/T)"
  (/ (comprimento-caminho caminho) *numero-nos-gerados*)
)

(defun comprimento-caminho (c)
  "Calcula o comprimento de um caminho"
  (- (length c) 1)
)

;; Fator de ramifica��o
(defun fator-ramificacao (comprimento-caminho total-nos a b tolerancia)
  "Calcula o fator de ramifica��o de um algoritmo (B + B^2 + � + B^L = T)"
  (metodo-bisseccao (lambda (b) (funcao-ramificacao b comprimento-caminho total-nos)) a b tolerancia)
)

(defun soma-potencias (ramificacao comprimento-caminho)
  "Realiza a soma de pot�ncias"
  (if (= comprimento-caminho  1)
      ramificacao
      (+ (expt ramificacao comprimento-caminho) (soma-potencias ramificacao (- comprimento-caminho 1)))))

(defun funcao-ramificacao (ramificacao comprimento-caminho total-nos)
  (- (soma-potencias ramificacao comprimento-caminho) total-nos))

(defun metodo-bisseccao (fun a b tolerancia)
  "Aplica o m�todo da bisse��o"
  (let ((fa (funcall fun a))
        (fb (funcall fun b)))
    (if (>= (* fa fb) 0)
        (error "A fun��o deve ter sinais opostos nos pontos a e b.")
        (bisseccao-recursiva fun a b fa fb tolerancia))))

(defun bisseccao-recursiva (fun a b fa fb tolerancia)
  (let* ((c (/ (+ a b) 2))
         (fc (funcall fun c)))
    (cond
      ((or (< (- b a) tolerancia) (= fc 0))
       c)
      ((< (* fa fc) 0)
       (bisseccao-recursiva fun a c fa fc tolerancia))
      (t
       (bisseccao-recursiva fun c b fc fb tolerancia)))))

;; Tempo execu��o
(defun medir-tempo (fun)
  "Mede o tempo de execu��o de uma fun��o (em segundos)"
  (let ((tempo-inicial (get-internal-real-time)))
    (funcall fun)
      (format t "Tempo Execu��o: ~,vF s ~%" 4 (float (/ (- (get-internal-real-time) tempo-inicial) internal-time-units-per-second)))))

(defun executar-bfs (estadoInicial)
  "Executa o algoritmo BFS com medi��o de tempo"
  (medir-tempo (lambda () (bfs estadoInicial))))

(defun executar-dfs (estadoInicial &optional (limite most-positive-fixnum))
  "Executa o algoritmo DFS com medi��o de tempo"
  (medir-tempo (lambda () (dfs estadoInicial limite))))

(defun executar-a-star (estadoInicial fHeuristica)
  "Executa o algoritmo A* com medi��o de tempo"
  (medir-tempo (lambda () (a-star estadoInicial fHeuristica))))
