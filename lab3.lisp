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



(defun check-bubble (name input expected)
  (format t "~:[FAILED~;passed~] ~a~%"
          (equal (bubble input) expected)
          name))

(defun check-badbubble (name input expected)
  (format t "~:[FAILED~;passed~] ~a~%"
          (equal (badbubble input) expected)
          name))



(defun test-bubble ()
  (check-bubble "test 1" '(1 2 3 4 5) '(1 2 3 4 5))  
  (check-bubble "test 2" '(5 4 3 2 1) '(1 2 3 4 5)) 
  (check-bubble "test 3" '(1 1 1 1 1) '(1 1 1 1 1))
  (check-bubble "test 4" '(9) '(9))
  (check-bubble "test 5" nil nil)
  (check-bubble "test 6" '(999 999 999 0 0 0) '(0 0 0 999 999 999))
  )

(defun test-badbubble ()
  (check-badbubble "test 1" '(1 2 3 4 5) '(1 2 3 4 5))  
  (check-badbubble "test 2" '(5 4 3 2 1) '(1 2 3 4 5)) 
  (check-badbubble "test 3" '(1 1 1 1 1) '(1 1 1 1 1))
  (check-badbubble "test 4" '(9) '(9))
  (check-badbubble "test 5" nil nil)
  (check-badbubble "test 6" '(999 999 999 0 0 0) '(0 0 0 999 999 999))
  )


  
