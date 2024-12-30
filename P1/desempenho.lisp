;;; Ficheiro: desempenho.lisp
;;; Funções para calcular as medidas de desempenho dos algoritmos
;;; Autores: João Fernandes e Rodrigo Santos

; Variável global para contar o número de nós gerados
(defparameter *numero-nos-gerados* 0.0)

(defun incrementar-nos ()
  "Incrementa o contador de nós gerados"
  (incf *numero-nos-gerados*))

(defun repor-contar-nos ()
  "Repõe a contagem de nós gerados"
  (setf *numero-nos-gerados* 0.0))

(defun apresentar-desempenho (c)
  "Apresenta o desempenho de um caminho"
  (format t "Número nós: ~A ~%" *numero-nos-gerados*)
  (format t "Comprimento caminho: ~A ~%" (comprimento-caminho c))
  (format t "Penetrância: ~,vF ~%" 2 (penetrancia c))
  (format t "Fator Ramificação: ~,vF ~%" 2 (float (fator-ramificacao (comprimento-caminho c) *numero-nos-gerados* 0 100 0.1)))
)

;; Penetrância
(defun penetrancia (caminho)
  "Calcula a penetrância de um algortimo (P = L/T)"
  (/ (comprimento-caminho caminho) *numero-nos-gerados*)
)

(defun comprimento-caminho (c)
  "Calcula o comprimento de um caminho"
  (- (length c) 1)
)

;; Fator de ramificação
(defun fator-ramificacao (comprimento-caminho total-nos a b tolerancia)
  "Calcula o fator de ramificação de um algoritmo (B + B^2 + … + B^L = T)"
  (metodo-bisseccao (lambda (b) (funcao-ramificacao b comprimento-caminho total-nos)) a b tolerancia)
)

(defun soma-potencias (ramificacao comprimento-caminho)
  "Realiza a soma de potências"
  (if (= comprimento-caminho  1)
      ramificacao
      (+ (expt ramificacao comprimento-caminho) (soma-potencias ramificacao (- comprimento-caminho 1)))))

(defun funcao-ramificacao (ramificacao comprimento-caminho total-nos)
  (- (soma-potencias ramificacao comprimento-caminho) total-nos))

(defun metodo-bisseccao (fun a b tolerancia)
  "Aplica o método da bisseção"
  (let ((fa (funcall fun a))
        (fb (funcall fun b)))
    (if (>= (* fa fb) 0)
        (error "A função deve ter sinais opostos nos pontos a e b.")
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

;; Tempo execução
(defun medir-tempo (fun)
  "Mede o tempo de execução de uma função (em segundos)"
  (let ((tempo-inicial (get-internal-real-time)))
    (funcall fun)
      (format t "Tempo Execução: ~,vF s ~%" 4 (float (/ (- (get-internal-real-time) tempo-inicial) internal-time-units-per-second)))))

(defun executar-bfs (estadoInicial)
  "Executa o algoritmo BFS com medição de tempo"
  (medir-tempo (lambda () (bfs estadoInicial))))

(defun executar-dfs (estadoInicial &optional (limite most-positive-fixnum))
  "Executa o algoritmo DFS com medição de tempo"
  (medir-tempo (lambda () (dfs estadoInicial limite))))

(defun executar-a-star (estadoInicial fHeuristica)
  "Executa o algoritmo A* com medição de tempo"
  (medir-tempo (lambda () (a-star estadoInicial fHeuristica))))
