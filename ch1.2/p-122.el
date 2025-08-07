;;;; 1.2.2 Древовидная рекурсия

					; Ряд Фибоначчи 0 1 1 2 3 5 8 13 21 ...
					; каждое следующее число = сумме предыдущих

					; (fib n) = 0, if n = 0
					;           1, if n = 1
					;           (fib (n-1)) (fib (n-2) - во всех остальных случаях

(defun fib (n)
  "Наивный способ определения числа Фибоначчи"
  (cond ((= n 0) 0)
	((= n 1) 1)
	(t (+ (fib (- n 1))
	      (fib (- n 2))))))

(fib 8) ; пример древовидной рекурсии: при увеличении аргумента число шагов растет экспотенциально, требования к памяти - линейно

(defun fib (n)
  "Итеративный способ определения числа Фибоначчи"
  (fib-iter 1 0 n))

(defun fib-iter (a b count)
  ""
  (if (= count 0)
      b
    (fib-iter (+ a b) a (- count 1))))

(fib 8) ; пример линейной итерации: при увеличении аргумента число шагов растет линейно

;;; Размен денег
					; a = 100 монеты 1, 5, 10, 25, 50 (n=5)

					; если сумма денег a=0, то имеется 1 способ размена
					;                  a<0             0
					; типов монет      n=0             0

(defun count-change (amount)
  "Принимает сумму AMOUNT и возвращает количество способов, которыми ее можно разменять, используя разные монеты"
  (cc amount 5))

(defun cc (amount kinds-of-coins)
  "Принимет сумму AMOUNT, количество видов монет KINDS-OF-COINS и возвращает количество вариантов размена"
  (cond ((= amount 0) 1)
	((or (< amount 0)
	     (= kinds-of-coins 0))
	 0)
	(t (+ (cc amount
		  (- kinds-of-coins 1))
	      (cc (- amount
		     (first-denomination kinds-of-coins))
		  kinds-of-coins)))))

(defun first-denomination (kinds-of-coins)
  "Принимает количество доступных типов монет KINDS-OF-COINS и возвращает достоинство максимальной монеты"
  (cond ((= kinds-of-coins 1) 1)
	((= kinds-of-coins 2) 5)
	((= kinds-of-coins 3) 10)
	((= kinds-of-coins 4) 25)
	((= kinds-of-coins 5) 50)))

(first-denomination 5)
(count-change 100)
(cc 100 3)

;; Упражнение 1.11

					; f(n)=n, if n<3
					; f(n)=f(n-1)+f(n-2)+f(n-3), if n>=3

(defun f (n)
  "Рекурсивная функция"
  (if (< n 3)
      n
    (+ (f (- n 1))
       (f (- n 2))
       (f (- n 3)))))

(f 0) ; 0
(f 1) ; 1
(f 2) ; 2
(f 3) ; 3
(f 4) ; 6
(f 5) ; 11
(f 6) ; 20
(f 7) ; 37

(defun f (n)
  "Итеративная функция"
  (if (<= n 3)
      n
    (f-iter 1 2 3 (- n 2))))

(f 0) ; 0
(f 1) ; 1
(f 2) ; 2
(f 3) ; 3
(f 4) ; 6
(f 5) ; 11
(f 6) ; 20
(f 7) ; 37

(defun f-iter (n1 n2 n3 count)
  ""
  (if (< count 0)
      (+ n1 n2 n3)
    (f-iter n2 n3 (+ n1 n2 n3) (- count 1))
    ))

(f-iter 1 2 3 0) ; 6
(f-iter 1 2 3 1) ; 6
(f-iter 1 2 3 2) ; 6
(f-iter 1 2 3 3) ; 11
(f-iter 1 2 3 4) ; 20

;;; Упражнение 1.12

(defun pascal (row num)
  "Рекурсивно вычисляет значение элемента треугольника Паскаля по его ряду ROW и номеру в ряду NUM. Счет идет от 0"
  (cond ((or (< num 0)
	     (> num row))
	     0)
	((or (= num 0)
	     (= num row))
	     1)
	(t (+ (pascal (- row 1) (- num 1))
	      (pascal (- row 1) num)))))

(pascal 4 2)
