;;; Ficheiro: desempenho.lisp
;;; Fun��es para calcular as medidas de desempenho dos algoritmos
;;; Autores: Jo�o Fernandes e Rodrigo Santos

;; Vari�vel global para contar o n�mero de n�s gerados
(defparameter *numero-nos-gerados* 0.0)

;; Fun��o para incrementar o contador de n�s gerados
(defun incrementar-nos ()
  (incf *numero-nos-gerados*))

(defun resetar-contar-nos ()
  "Reseta o contador de n�s gerados para 0."
  (setf *numero-nos-gerados* 0.0))

(defun apresentar-desempenho (c)
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
  (- (length c) 1)
)

(defun total-nos (abertos fechados)
  (+ (length abertos) (length fechados))
)

;; Fator de ramifica��o
(defun fator-ramificacao (comprimento-caminho total-nos a b tolerancia)
  "Calcula o fator de ramifica��o de um algoritmo (B + B^2 + � + B^L = T)"
  (metodo-bisseccao (lambda (b) (funcao-f b comprimento-caminho total-nos)) a b tolerancia)
)

(defun soma-potencias (ramificacao comprimento-caminho)
  (if (= comprimento-caminho  1)
      ramificacao
      (+ (expt ramificacao comprimento-caminho) (soma-potencias ramificacao (- comprimento-caminho 1)))))

(defun funcao-f (ramificacao comprimento-caminho total-nos)
  (- (soma-potencias ramificacao comprimento-caminho) total-nos))

(defun metodo-bisseccao (fun a b tolerancia)
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
  (let ((tempo-inicial (get-internal-real-time)))
    (funcall fun)
      (format t "Tempo Execu��o: ~,vF s ~%" 4 (float (/ (- (get-internal-real-time) tempo-inicial) internal-time-units-per-second)))))

(defun executar-bfs (estadoInicial)
  (medir-tempo (lambda () (bfs estadoInicial))))

(defun executar-dfs (estadoInicial &optional (limite most-positive-fixnum))
  (medir-tempo (lambda () (dfs estadoInicial limite))))

(defun executar-a-star (estadoInicial fHeuristica)
  (medir-tempo (lambda () (a-star estadoInicial fHeuristica))))
