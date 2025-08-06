;;;; 1.1.7 Пример: вычисление квадратного корня методом Ньютона"

(defun square (x)
  "Возвращает квадрат X"
  (* x x))

(square 2)

(defun good-enough (guess x)
  "Проверяет совпадение квадрата GUESS и X в пределах допуска"
  (< (abs
      (- (square guess)
	 x))
     0.001))

(good-enough 1.0001 1)

(defun average (x y)
  "Получает среднее двух чисел X и Y"
  (/ (+ x y) 2.0))

(average 1 2)

(defun improve (guess x)
  "Получает приближение числа GUESS к корню из X"
  (average guess (/ x guess)))

(improve 1.0 2.0)

(defun sqrt-iter (guess x)
  "Получает приближение числа GUESS к корню из  X итеративно"
  (if (good-enough guess x)
      guess
    (sqrt-iter (improve guess x)
	       x))
  )
  
(sqrt-iter 1 4)

(defun sqrt (x)
  "Возвращает квадратный корень числа X"
  (sqrt-iter 1.0 x))

(sqrt 9)
(sqrt (+ 100 37))
(sqrt (+ (sqrt 2) (sqrt 3)))
(square (sqrt 1000))

;; Упражнение 1.6

(defun new-if (predicate then-clause else-clause)
  "Замена IF при помощи COND"
  (cond (predicate then-clause) ; cond - процедура и вычисляет все ветки перед выполнением
	(else-clause)))         ; в нормальном if эта ветка не вычисляется, если первое условие верно

(new-if (= 2 3) 0 5)
(new-if (= 1 1) 0 5)

(defun new-sqrt-iter (guess x)
  "Получает приближение числа GUESS к корню из  X итеративно"
  (new-if (good-enough guess x)
	  guess
	  (new-sqrt-iter (improve guess x) 
			 x))
  )
  
(new-sqrt-iter 1 4) ; уходит в рекурсию

;; Упражнение 1.7

(defun good-enough2 (guess-old guess-new)
  "Проверяет совпадение старого GUESS-OLD и нового GUESS-OLD значений приближений в пределах допуска"
  (< (abs
      (- guess-new guess-old))
     0.001))

(good-enough2 1.3 1.4)

(defun sqrt-iter2 (guess-old guess-new x)
  "Получает приближение числа GUESS к корню из  X итеративно"
  (if (good-enough2 guess-old guess-new)
      guess-new
    (sqrt-iter2 guess-new (improve guess-new x) x))
  )
  
(sqrt-iter2 0.5 1 4)      ; 2.0000000929222947
(sqrt-iter 1 4)           ; 2.0000000929222947
    
(defun sqrt2 (x)
  "Возвращает квадратный корень числа X"
  (sqrt-iter2 0.5 1.0 x))

(sqrt 9)                  ; 3.00009155413138
(sqrt2 9)                 ; 3.000000001396984 => !точнее

;; Упражнение 1.8

(defun cube (x)
  "Возвращает куб X"
  (* x x x))

(cube 2)

(defun good-enough-cube (guess x)
  "Проверяет совпадение куба GUESS и X в пределах допуска"
  (< (abs
      (- (cube guess)
	 x))
     0.001))

(good-enough-cube 1.0001 2)

(defun improve-cube (guess x)
  "Получает приближение числа GUESS к кубическому корню из X"
  (/ (+ (/ x (square guess))
	(* 2 guess))
     3))

(improve-cube 1.0 2.0)

(defun cube-iter (guess x)
  "Получает приближение числа GUESS к корню из  X итеративно"
  (if (good-enough-cube guess x)
      guess
    (cube-iter (improve-cube guess x)
	       x))
  )

(cube-iter 1.0 27)

(defun cube2 (x)
  "Возвращает кубический корень числа X"
  (cube-iter 1.0 x))

(cube2 27.0)
