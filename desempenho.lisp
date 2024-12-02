;;; Ficheiro: desempenho.lisp
;;; Fun��es para calcular as medidas de desempenho dos algoritmos
;;; Autores: Jo�o Fernandes e Rodrigo Santos

(defun penetrancia (no-final abertos fechados tamanho-espaco-busca)
  "Calcula a penetr�ncia dado o n� final, listas de abertos e fechados, e o tamanho do espa�o de busca."
  (let* ((n-nos-visitados (+ (length abertos) (length fechados)))  ; Total de n�s visitados
         (penetrancia (/ (* 100.0 n-nos-visitados) tamanho-espaco-busca)))  ; Penetr�ncia
    (list :no-final no-final               ; Retorna o n� final
          :n-nos-visitados n-nos-visitados ; N�s visitados
          :tamanho-espaco-busca tamanho-espaco-busca ; Espa�o de busca
          :penetrancia (round penetrancia)))) ; Penetr�ncia arredondada


(defun tamanho-espaco-busca (tabuleiro)
  "Calcula o tamanho do espa�o de busca do tab"
  (fatorial (length (flatten tabuleiro))))

(defun fatorial (n)
  "Calcula o fatorial de n."
  (if (<= n 1)
      1
      (* n (fatorial (- n 1)))))

(defun fator-ramificacao (L total-nos &key (accuracy 0.0001))
  "Calcula o fator de ramifica��o m�dio B usando Newton-Raphson."
  (let* ((f (lambda (B)
              "Fun��o f(B): soma dos termos - total-nos."
              (- (loop for i from 1 to L
                       sum (expt B i))
                 total-nos)))
         (f-prime (lambda (B)
                    "Derivada f'(B): soma das derivadas dos termos."
                    (loop for i from 1 to L
                          sum (* i (expt B (1- i)))))))
    (multiple-value-bind (B iteracoes)
        (Newton-Raphson f f-prime 0.1 10.0 :accuracy accuracy)
      (format t "Fator de ramifica��o m�dio calculado: ~a (em ~d itera��es)~%"
              B iteracoes)
      B)))


(defun Newton-Raphson
       (f
        f-prime
        x-left
        x-right
        &key
        (accuracy (* 10.0 single-float-epsilon))
        (maximum-number-of-iterations 20)
        (prevent-bracket-jumping-p t))
 (assert (< x-left x-right))
  (let ((x (* 0.5 (+ x-left x-right)))
        delta-x denom-for-accuracy-test)
    (dotimes (j maximum-number-of-iterations
                (if (not (cerror "returns solution so far"
                                 "exceeding maximum number of iterations"))
                  (values x maximum-number-of-iterations)))
      (setf delta-x (/ (funcall f x)  (funcall f-prime x)))
      (setf denom-for-accuracy-test (+ (abs x)
                                       (abs (decf x delta-x))))
      (cond
       (prevent-bracket-jumping-p
        (if (< x x-left) (setf x x-left))
        (if (> x x-right) (setf x x-right))
        (if (< (/ (abs delta-x) denom-for-accuracy-test) accuracy)
          (return (values x (1+ j)))))
       ((<= x-left x x-right)
        (if (< (/ (abs delta-x) denom-for-accuracy-test) accuracy)
          (return (values x (1+ j)))))
       (t
        (error "jumped out of brackets")
        )))))