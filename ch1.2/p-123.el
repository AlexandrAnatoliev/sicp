;;;; 1.2.3 Порядки роста

;;; Упражнение 1.14

(defun count-change (amount)
  "Принимает сумму AMOUNT и возвращает количество способов, которыми ее можно разменять, используя разные монеты"
  (cc amount 5))

(defun cc (amount kinds-of-coins)
  "Принимет сумму AMOUNT, количество видов монет KINDS-OF-COINS и возвращает количество вариантов размена"
  (cond ((= amount 0)
	 (message "answer +1")
	 1)
	((or (< amount 0)
	     (= kinds-of-coins 0))
	 0)

	(t (message "(cc %d %d)" amount kinds-of-coins)
	   (message "(cc %d %d) + (cc %d %d)" amount (- kinds-of-coins 1) (- amount (first-denomination kinds-of-coins)) kinds-of-coins)
	   (+ (cc amount
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

(count-change 11) ; количество шагов растет экспоненциально, расход памяти - линейно
					; (cc 11 5)
					; (cc 11 4) + (cc -39 5)
					; (cc 11 4)
					; (cc 11 3) + (cc -14 4)
					; (cc 11 3)
					; (cc 11 2)                                                                 + (cc 1 3)
					; (cc 11 2)                                                                   (cc 1 3)
					; (cc 11 1)               + (cc 6 2)                                          (cc 1 2) + (cc -9 3)
					; (cc 11 1)                 (cc 6 2)                                          (cc 1 2)
					; (cc 11 0) + (cc 10 1)     (cc 6 1)               + (cc 1 2)                 (cc 1 1) + (cc -4 2)
					; (cc 10 1)                 (cc 6 1)                 (cc 1 2)                 (cc 1 1)
					; (cc 10 0) + (cc 9 1)      (cc 6 0) + (cc 5 1)      (cc 1 1) + (cc -4 2)     (cc 1 0) + (cc 0 1)
					; (cc 9 1)                  (cc 5 1)                 (cc 1 1)                 answer +1
					; (cc 9 0) + (cc 8 1)       (cc 5 0) + (cc 4 1)      (cc 1 0) + (cc 0 1)
					; (cc 8 1)                  (cc 4 1)                 answer +1
					; (cc 8 0) + (cc 7 1)       (cc 4 0) + (cc 3 1)
					; (cc 7 1)                  (cc 3 1)
					; (cc 7 0) + (cc 6 1)       (cc 3 0) + (cc 2 1)
					; (cc 6 1)                  (cc 2 1)
					; (cc 6 0) + (cc 5 1)       (cc 2 0) + (cc 1 1)
					; (cc 5 1)                  (cc 1 1)
					; (cc 5 0) + (cc 4 1)       (cc 1 0) + (cc 0 1)
					; (cc 4 1)                  answer +1
					; (cc 4 0) + (cc 3 1)
					; (cc 3 1)
					; (cc 3 0) + (cc 2 1)
					; (cc 2 1)
					; (cc 2 0) + (cc 1 1)
					; (cc 1 1)
					; (cc 1 0) + (cc 0 1)
					; answer +1


;;; Упражнение 1.15

(defun cube (x)
  "Функция возвращает X^3"
  (* x x x))

(cube 3)

(defun p (x)
  ""
  (- (* 3 x)
     (* 4 (cube x))))

(p 3)

(defun sine (angle)
  "Вычисляем синус угла ANGLE (рад)"
  (if (not (> (abs angle) 0.1))
      angle
    (message "+")
    (p (sine (/ angle 3.0)))))

(sine 12.15) ; процедура P вызывается 5 раз
(sine 10) ; 5 раз
(sine 20) ; 5
(sine 30) ; 6
(sine 100) ; 7
(sine 1000) ; 9
					; при увеличении ANGLE количество операций возрастает логарифмически, количество используемой памяти - линейно?

