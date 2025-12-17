<p align="center"><b>МОНУ НТУУ КПІ ім. Ігоря Сікорського ФПМ СПіСКС</b></p>
<p align="center">
<b>Звіт з лабораторної роботи 3</b><br/>
"Конструктивний і деструктивний підходи до роботи зі списками"<br/>
дисципліни "Вступ до функціонального програмування"
</p>
<p align="right"><b>Студент(-ка)</b>: Кукса Кирило Віталійович</p>
<p align="right"><b>Рік</b>: 2025</p>

## Загальне завдання
 Реалізуйте алгоритм сортування чисел у списку двома способами: функціонально і
імперативно.
1. Функціональний варіант реалізації має базуватись на використанні рекурсії і
конструюванні нових списків щоразу, коли необхідно виконати зміну вхідного
списку. Не допускається використання: псевдо-функцій, деструктивних операцій,
циклів . Також реалізована функція не має бути функціоналом (тобто приймати на
вхід функції в якості аргументів).

2. Імперативний варіант реалізації має базуватись на використанні циклів і
деструктивних функцій (псевдофункцій). Не допускається використання функцій
вищого порядку або функцій для роботи зі списками/послідовностями, що
використовуються як функції вищого порядку. Тим не менш, оригінальний список
цей варіант реалізації також не має змінювати, тому перед виконанням
деструктивних змін варто застосувати функцію copy-list (в разі необхідності).
Також реалізована функція не має бути функціоналом (тобто приймати на вхід
функції в якості аргументів).
## Варіант 8 (3)
Алгоритм сортування обміном No2 (із використанням прапорця) за незменшенням.
## Лістинг функції з використанням конструктивного підходу
```lisp
(defun swap-pass (lst)
  (cond
    ((or (null lst) (null (rest lst)))
     (values lst nil))

    ((> (first lst) (second lst))
     (multiple-value-bind (rest-list flag)
         (swap-pass (cons (first lst) (rest (rest lst))))
       (values (cons (second lst) rest-list) t)))

    (t
     (multiple-value-bind (rest-list flag)
         (swap-pass (rest lst))
       (values (cons (first lst) rest-list) flag)))))



(defun bubble (lst)
  (multiple-value-bind (new-list flag) (swap-pass lst)
    (if flag
        (bubble new-list)
        new-list)))
```
### Тестові набори та утиліти
```lisp
(defun check-bubble (name input expected)
  (format t "~:[FAILED~;passed~] ~a~%"
          (equal (bubble input) expected)
          name))


(defun test-bubble ()
  (check-bubble "test 1" '(1 2 3 4 5) '(1 2 3 4 5))  
  (check-bubble "test 2" '(5 4 3 2 1) '(1 2 3 4 5)) 
  (check-bubble "test 3" '(1 1 1 1 1) '(1 1 1 1 1))
  (check-bubble "test 4" '(9) '(9))
  (check-bubble "test 5" nil nil)
  (check-bubble "test 6" '(999 999 999 0 0 0) '(0 0 0 999 999 999))
  )
```
### Тестування
```lisp
CL-USER> (test-bubble)
passed test 1
passed test 2
passed test 3
passed test 4
passed test 5
passed test 6
NIL
```
## Лістинг функції з використанням деструктивного підходу
```lisp
(defun badbubble (lst)
  (let* ((a (copy-list lst))
         (n (length a))
         (flag t))

    (loop while flag do
          (setf flag nil)
          (dotimes (i (1- n))
            (when (> (nth i a) (nth (1+ i) a))
              (rotatef (nth i a) (nth (1+ i) a))
              (setf flag t))))

    a))
```
### Тестові набори та утиліти
```lisp
(defun check-badbubble (name input expected)
  (format t "~:[FAILED~;passed~] ~a~%"
          (equal (badbubble input) expected)
          name))


(defun test-badbubble ()
  (check-badbubble "test 1" '(1 2 3 4 5) '(1 2 3 4 5))  
  (check-badbubble "test 2" '(5 4 3 2 1) '(1 2 3 4 5)) 
  (check-badbubble "test 3" '(1 1 1 1 1) '(1 1 1 1 1))
  (check-badbubble "test 4" '(9) '(9))
  (check-badbubble "test 5" nil nil)
  (check-badbubble "test 6" '(999 999 999 0 0 0) '(0 0 0 999 999 999))
  )
```

### Тестування
```lisp
CL-USER> (test-badbubble)
passed test 1
passed test 2
passed test 3
passed test 4
passed test 5
passed test 6
NIL
```