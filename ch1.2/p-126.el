;;;; 1.2.6 Пример:  проверка на простоту

;;; Поиск делителей

(defun smallest-divisor (n)
  "Вычисляет наименьший простой делитель числа N перебором"
  (find-divisor n 2))

(defun find-divisor (n test-divisor)
  "TEST-DIVISOR - число с которого начинается поиск делителя, N - проверяемое число"
  (cond ((> (square test-divisor) n) n)
	((divides test-divisor n) test-divisor)
	(t (find-divisor n (+ test-divisor 1)))))

(find-divisor 13 2)

(defun square (x)
  "Вернет X^2"
  (* x x))

(square 5)

(defun divides (a b)
  "Вернет t в случае делимости числа B на A"
  (= (mod b a) 0))

(divides 2 4)
(divides 2 5)

(smallest-divisor 25)

(defun prime (n)        ; сложность O(sqrt(N))
  "Вернет t, если число N является простым"
  (= n (smallest-divisor n)))

(prime 13)

;;; Тест Ферма


					; 2^3 = 8
					; 8%3 = 2
					; 2%3 = 2

(defun expmod (base exp m)
  "Процедура вычисляет степень числа по модулю другого числа (BASE^EXP)%M"
  (cond ((= exp 0) 1)
	((even exp)
	 (mod (square (expmod base (/ exp 2) m))
	      m))
	(t
	 (mod (* base (expmod base (- exp 1) m))
	      m))))

(expmod 1 2 3) ; (1^2)%3 = 1

(defun even (x)
  "Проверяет число X на четность"
  (= (mod x 2) 0))

(even 19)

(defun fermat-test (n)
  "Процедура выбирает случайное число между 1 и (N-1)"
  (defun try-it (a)
    "...и этим случайным числом (с помощью теоремы Ферма) проверяет,
является ли число N простым"
    (= (expmod a n n) a))
  (try-it (+ (random (- n 1)))))

(fermat-test 5123)

(defun fast-prime (n times)
  "Процедура проверяет число N на простоту с помощью теоремы Ферма
TIMES раз"
  (cond ((= times 0) t)
	((fermat-test n)
	 (fast-prime n (- times 1)))
	(t nil)))

(fast-prime 2624911 50)

;;; Упражнение 1.21

(smallest-divisor 199)   ; 199
(smallest-divisor 1999)  ; 1999
(smallest-divisor 19999) ; 7

;;; Упражнение 1.22

(defun timed-prime-test (n)
  "Если число N - простое, выведет *** и время, затраченное на проверку"
  (newline)
  (princ "; " (current-buffer))
  (princ n (current-buffer))
  (start-prime-test n (float-time)))

(timed-prime-test 1999) 
; 1999 *** 9.846687316894531e-05

(defun start-prime-test (n start-time)
  "Проверяет число N на простоту и замеряет время исполнения программы"
  (if (prime n)
      (report-prime (- (float-time) start-time))))

(defun report-prime (elapsed-time)
  "Вывод вычислений"
  (princ " *** " (current-buffer))
  (princ elapsed-time (current-buffer)))

(defun search-for-primes (start count)
  "Находит наименьшие COUNT простых чисел после START, выводит их и
  время выполнения программы"
  (while (> count 0)
    (if (prime start)
	(setq count (- count 1)))
    (if (prime start)
	(timed-prime-test start))	
    (setq start (+ start 1))))

(search-for-primes 1000 3)
; 1009 *** 4.2438507080078125e-05
; 1013 *** 3.457069396972656e-05
; 1019 *** 3.886222839355469e-05

(search-for-primes 10000 3)
; 10007 *** 0.00013446807861328125
; 10009 *** 0.00011491775512695312
; 10037 *** 0.00010585784912109375

(search-for-primes 100000 3)
; 100003 *** 0.00035190582275390625
; 100019 *** 0.0003693103790283203
; 100043 *** 0.0003523826599121094

(search-for-primes 1000000 3) ; превышает пределы глубины рекурсии
					; увеличение чисел в 10 раз увеличивает время на sqrt(10)

;;; Упражнение 1.23

(defun next (x)
  "Процедура для получения нечетных чисел"
  (if (= x 2)
      3
    (+ x 2)))

(next 3)

(defun find-divisor (n test-divisor)
  "TEST-DIVISOR - число с которого начинается поиск делителя, N - проверяемое число"
  (cond ((> (square test-divisor) n) n)
	((divides test-divisor n) test-divisor)
	(t (find-divisor n (next test-divisor)))))

(find-divisor 123 2)
  
(search-for-primes 1000 3)
; 1009 *** 2.9087066650390625e-05 new
; 1013 *** 2.4080276489257812e-05
; 1019 *** 2.3603439331054688e-05 old
; 1009 *** 4.2438507080078125e-05
; 1013 *** 3.457069396972656e-05
; 1019 *** 3.886222839355469e-05

(search-for-primes 10000 3)
; 10007 *** 7.367134094238281e-05 new
; 10009 *** 6.985664367675781e-05
; 10037 *** 6.794929504394531e-05
; 10007 *** 0.00013446807861328125 old
; 10009 *** 0.00011491775512695312
; 10037 *** 0.00010585784912109375

(search-for-primes 100000 3)
; 100003 *** 0.00023484230041503906 new
; 100019 *** 0.00021314620971679688
; 100043 *** 0.0002129077911376953
; 100003 *** 0.00035190582275390625 old
; 100019 *** 0.0003693103790283203
; 100043 *** 0.0003523826599121094

(search-for-primes 1000000 3)
; 1000003 *** 0.0006844997406005859
; 1000033 *** 0.0006725788116455078
; 1000037 *** 0.0006630420684814453

					; почти двукратное увеличение скорости работы программы

;;; Упражнение 1.24

(defun start-prime-test (n start-time)
  "Проверяет число N на простоту и замеряет время исполнения программы"
  (if (fast-prime n 3)
      (report-prime (- (float-time) start-time))))

(search-for-primes 1000 3)
; 1009 *** 9.918212890625e-05     new
; 1013 *** 8.034706115722656e-05
; 1019 *** 8.296966552734375e-05
; 1009 *** 4.2438507080078125e-05 old
; 1013 *** 3.457069396972656e-05
; 1019 *** 3.886222839355469e-05

(search-for-primes 10000 3)
; 10007 *** 0.00012683868408203125 new
; 10009 *** 8.606910705566406e-05
; 10037 *** 8.940696716308594e-05
; 10007 *** 0.00013446807861328125 old
; 10009 *** 0.00011491775512695312
; 10037 *** 0.00010585784912109375

(search-for-primes 100000 3)
; 100003 *** 0.0001232624053955078  new
; 100019 *** 0.00010275840759277344
; 100043 *** 0.00010228157043457031
; 100003 *** 0.00035190582275390625  old
; 100019 *** 0.0003693103790283203
; 100043 *** 0.0003523826599121094

(search-for-primes 1000000 3)
; 1000003 *** 0.00016260147094726562 new
; 1000033 *** 0.00014328956604003906
; 1000037 *** 4.1484832763671875e-05

				       			       
					; увеличение чисел в 1000 раз увеличивает время в 2 раза

;;; Упражнение 1.25

(defun expmode (base exp m)
  "Процедура вычисляет степень числа по модулю другого числа (BASE^EXP)%M упрощенная"
  (mod (fast-expt base expt) m))

(fermat-test 10007)   ; t
(fermat-test 100043)  ; t
(fermat-test 1000037) ; t

;;; Упражнение 1.26

(defun expmode (base exp m)
  "Процедура вычисляет степень числа по модулю другого числа
(BASE^EXP)%M со сложностью O(N)"
  (cond ((= exp 0) 1)
	((even exp)
	 (mod (* (expmode base (/ exp 2) m)
		 (expmode base (/ exp 2) m))
	      m))
	(t
	 (mod (* base 9(expmode base (- exp 1) m))
	      m))))
		 
(fermat-test 10007)   ; t
(fermat-test 100043)  ; t
(fermat-test 1000037) ; t

;;; Упражнение 1.27

(defun karmake-test (count n)
  "Процедура берет все числа между 1 и (N-1) и проверяет на простоту с
помощью теоремы Ферма"
  (cond ((= count 0) t)
	((not (= (expmod count n n)
		(mod count n)))
	 nil)
	(t
	 (karmake-test (- count 1) n))))

(karmake-test 560 561)
(karmake-test 999 1000)

  
