;;; Ficheiro: desempenho.lisp
;;; Funções para calcular as medidas de desempenho dos algoritmos
;;; Autores: João Fernandes e Rodrigo Santos

;; Variável global para contar o número de nós gerados
(defparameter *numero-nos-gerados* 0.0)

;; Função para incrementar o contador de nós gerados
(defun incrementar-nos ()
  (incf *numero-nos-gerados*))

(defun resetar-contar-nos ()
  "Reseta o contador de nós gerados para 0."
  (setf *numero-nos-gerados* 0.0))

(defun apresentar-desempenho (c)
  (format t "Número nós: ~A ~%" *numero-nos-gerados*)
  (format t "Comprimento caminho: ~A ~%" (comprimento-caminho c))
  (format t "Penetrância: ~A ~%" (penetrancia c))
  (format t "Fator Ramificação: ~A ~%" (fator-ramificacao (comprimento-caminho c) *numero-nos-gerados* 1 (comprimento-caminho c) 0.0001))
)

;; Penetrância
(defun penetrancia (caminho)
  "Calcula a penetrância de um algortimo (P = L/T)"
  (/ (comprimento-caminho caminho) *numero-nos-gerados*)
)

(defun comprimento-caminho (c)
  (- (length c) 1)
)

(defun total-nos (abertos fechados)
  (+ (length abertos) (length fechados))
)

;; Fator de ramificação
(defun fator-ramificacao (comprimento-caminho total-nos a b tolerancia)
  "Calcula o fator de ramificação de um algoritmo (B + B^2 + … + B^L = T)"
  (metodo-bisseccao (lambda (b) (funcao-f b comprimento-caminho total-nos)) a b tolerancia)
)

(defun soma-potencias (ramificacao comprimento-caminho)
  (if (= comprimento-caminho  1)
      ramificacao
      (+ (expt ramificacao comprimento-caminho) (soma-potencias ramificacao (- comprimento-caminho 1)))))

(defun funcao-f (ramificacao comprimento-caminho total-nos)
  (- (soma-potencias ramificacao comprimento-caminho) total-nos))

(defun metodo-bisseccao (fun a b tolerancia)
  (if (< (- b a) tolerancia)(/ (+ a b) 2)
      (let* ((c (/ (+ a b) 2))
             (fa (funcall fun a))
             (fc (funcall fun c)))
        (cond
            ((= fc 0) c)
            ((< (* fa fc) 0) (metodo-bisseccao fun c b tolerancia))
            (t (metodo-bisseccao fun a c tolerancia)))))
)

;; Tempo execução
(defun medir-tempo (fun)
  (let ((tempo-inicial (get-internal-real-time)))
    (funcall fun)
    (let ((tempo-final (get-internal-real-time)))
      (format t "Tempo Execução: ~A s ~%" (float (/ (- tempo-final tempo-inicial) internal-time-units-per-second))))))

(defun executar-bfs (estadoInicial)
  (medir-tempo (lambda () (bfs estadoInicial))))

(defun executar-dfs (estadoInicial &optional (limite most-positive-fixnum))
  (medir-tempo (lambda () (dfs estadoInicial limite))))

(defun executar-a-star (estadoInicial fHeuristica)
  (medir-tempo (lambda () (a-star estadoInicial fHeuristica))))
