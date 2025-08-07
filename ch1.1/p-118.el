;;;; 1.1.8 Процедуры как абстракции типа "Черный ящик"

(defun square (x)              ; разные реализации
  "Возвращает квадрат X"
  (* x x))

(defun square (x)              ; одной и той же функции
  "Возвращает квадрат X"       ; неотличимы друг от друга:
  (exp (double (log x))))      ; принимают и возвращают одни и те же параметры
                               ; что прозволяет рассматривать их как "черный ящик"
(defun double (x)
  "Возвращает удвоенный X"
  (+ x x))

(double 2)
  
(square 2)

;;; Локальные имена

(defun square (x)              ; вне зависимости от выбранных имен для формальных параметров 
  "Принимает локальный параметр X и возводит его в квадрат"
  (* x x))

(defun square (y)              ; эти функции неотличимы
  "Принимает локальный параметр Y и возводит его в квадрат"
  (* y y))

(square 2)

(defun good-enough (guess x)   ; эта переменная x 
  (< (abs
      (- (square  guess)       ; не смешивается с x, принимаемой square
	 x))
     0.001))

					; Определение процедуры связывает свои параметры
					; good-enough, guess, x - связанные переменные - работают в одной области действия
					; <, abs, square, - свободные

;;; Внутренние определения и блочная структура

(defun square (x)                                                 ; программа sqrt состоит из ряда процедур (функций)
  "Возвращает квадрат X"
  (* x x))

(defun good-enough (guess x)                                      ; пользователю программы sqrt
  "Проверяет совпадение квадрата GUESS и X в пределах допуска"
  (< (abs
      (- (square guess)
	 x))
     0.001))

(defun average (x y)                                              ; незачем знать какие программы она использует для своей работы
  "Получает среднее двух чисел X и Y"
  (/ (+ x y) 2.0))

(defun improve (guess x)                                          ; однако при больших системах возможно провторное создание функции с таким же названием для других нужд 
  "Получает приближение числа GUESS к корню из X"
  (average guess (/ x guess)))

(defun sqrt-iter (guess x)                                        ; что сломает работу данной программы sqrt
  "Получает приближение числа GUESS к корню из  X итеративно"
  (if (good-enough guess x)
      guess
    (sqrt-iter (improve guess x)
	       x))
  )
  
(defun sqrt (x)
  "Возвращает квадратный корень числа X"
  (sqrt-iter 1.0 x))

(sqrt 9)

;; Проблема решается созданием внутри процедуры локальных процедур

(defun sqtr (x)                                                   ; пример блочной структуры
  "программа вычисляет квадратный корень числа X, используя локальные процедуры"
  (defun square (x)    
    (* x x))
  (defun good-enough (guess x)
    (< (abs
	(- (square guess)
	   x))
       0.001))
  (defun average (x y)
    (/ (+ x y) 2.0))
  (defun improve (guess x)
    (average guess (/ x guess)))
  (defun sqrt-iter (guess x)    
    (if (good-enough guess x)
	guess
      (sqrt-iter (improve guess x)
		 x))
    )
  (sqrt-iter 1.0 x))

(sqrt 9)

(defun sqtr (x)                                                   ; пример блочной структуры
  "программа вычисляет квадратный корень числа X, используя локальные процедуры"
  (defun square (x)                     ; используем X определенную для SQRT как свободную переменную внутри программы
    (* x x))                            ; упрощаем программу:
  (defun good-enough (guess)            ; не передаем явно сюда X
    (< (abs
	(- (square guess)
	   x))
       0.001))
  (defun average (x y)
    (/ (+ x y) 2.0))
  (defun improve (guess)                ; и сюда
    (average guess (/ x guess)))
  (defun sqrt-iter (guess)              ; и сюда
    (if (good-enough guess)
	guess
      (sqrt-iter (improve guess)
		 x))
    )
  (sqrt-iter 1.0))                      ; и сюда - это лексическая сфера дйствия переменных

(sqrt 9)

