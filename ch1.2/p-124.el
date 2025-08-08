;;;; 1.2.4 Возведение в степень

					; b^n = b*b^(n-1)
					; b^0 = 1

(defun expt (b n)
  "Возвращает B^N рекурсивно"
  (if (= n 0)
      1
    (* b (expt b (- n 1)))))

(expt 2 4)

					; Линейно рекурсивный процесс, коичество шагов и требуемой памяти пропорционально N

(defun expt (b n)
  "Возвращает B^N итеративно"
  (expt-iter b n 1))

(defun expt-iter (b counter product)
  ""
  (if (= counter 0)
      product
    (expt-iter b
	       (- counter 1)
	       (* b product))))

(expt 2 4)

					; Линейно итеративный подход, требует O(N) шагов и O(1) памяти

;;; Последовательное возведение в квадрат

					; Чтобы вычислить b^8
					; вместо b*b*b*b*b*b*b*b
					; все вычислить можно за три умножения:
					; b^2 = b*b
					; b^4 = b^2 * b^2
					; b^8 = b^4 * b^4

					; Чтобы вычислить любую степень протребуется два правила:
					; b^n = (b^(n/2))^2 if четно
					; b^n = b * b^(n-1) if нечетно

(defun fast-expt (b n)
  "Быстрый алгоритм получения B^N"
  (cond ((= n 0) 1)
	((even n) (square (fast-expt b (/ n 2))))
	(t (* b (fast-expt b (- n 1))))))

(fast-expt 2 7)

					; Использование этого алгоритма имеет сложность O(log(N)) по количеству требуемых шагов и требуемой памяти

(defun even (n)
  "Возвращает t в случае если N - четное число"
  (= (mod n 2) 0))

(even 4)

(defun square (n)
  "Возвращает N^2"
  (* n n))

(square 4)

;;; Упражнение 1.16

					; a*b^n = b^n
					; начальные параметры:
					; a = 1
					; b - основание степени
					; n - показатель степени

					; если n - четное:
					; b^n = (b^2)^(n/2) -> b = b^2, n = n/2, a - не меняется
					; если n - нечетное:
					; a*b^n = a*b*b^(n-1) -> n = n-1, a = a*b
					; когда n=0, взвращаем a, т.к. a*b^0 = a

(defun fast-expt (b n)
  "Итеративная версия быстрого алгоритма возведения числа в степень B^N"
  (fast-expt-iter 1 b n))

(fast-expt 2 7)

(fast-expt-iter 1 2 4)

(defun fast-expt-iter (a b n)
  ""
  (if (= n 0)
      a
    (cond ((= n 0) a)
	  ((even n) (setq b (square b)
			n (/ n 2)))
	  (t (setq n (- n 1)
		   a (* a b))))
    (fast-expt-iter a b n)))
  
;;; Упражнение 1.17

					; Данный алгоритм имеет сложность O(B)

(defun mult (a b)
  "Операция умножения A на B, реализованная при помощи сложения"
  (if (= b 0)
      0
    (+ a (mult a (- b 1)))))

(mult 4 5)

					; Данный алгоритм имеет сложность O(log(B))

(defun double (x)
  "Вернет 2*X"
  (* 2 x))

(double 2)

(defun halve (x)
  "Делит четное число X пополам"
  (/ x 2))

(halve 2)

(defun fast-mult (a b)
  "Быстрый алгоритм умножения чисел A и B рекурсивным методом"
  (cond ((= b 1) a)
	((even b)
	 (fast-mult (double a)
			     (halve b)))
	(t
	 (+ a (fast-mult a
			   (- b 1))))))

(fast-mult 7 123)

;;; Упражнение 1.18

(defun fast-mult-iter (a b)
  "Быстрый алгоритм умножения чисел A и B итеративным способом"
  (mult-iter 0 a b))

(fast-mult-iter 5 6)

(defun mult-iter (answer a b)
  ""
  (if (= b 1)
      (setq answer (+ answer a))
    
    (if (even b) (setq a (double a)
		       b (halve b))
      (setq answer (+ answer a)
	    b (- b 1)))
    (mult-iter answer a b)))

(mult-iter 0 12 23)

;;; Упражнение 1.19

(defun fib (n)
  "Итеративный алгоритм вычисления N-го числа Фибоначчи за логарифмическое время"
  (fib-iter 1 0 0 1 n))

(defun fib-iter (a b p q count)
  ""
  (cond ((= count 0) b)
	((even count)
	 (fib-iter a
		   b
		   (+ (* p p) (* q q))   ; p' = p^2 + q^2
		   (+ (* q q) (* 2 p q)) ; q' = q^2 + 2*p*q
		   (/ count 2)))
	(t (fib-iter (+ (* b q) (* a q) (* a p))
		     (+ (* b p) (* a q))
		     p
		     q
		     (- count 1)))))

(fib 6)
		      
	
	 
