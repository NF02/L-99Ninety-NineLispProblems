(defun insert-at (element lst k)
  (if (<= k 1)
      (cons element lst)
    (cons (car lst) (insert-at element (cdr lst) (- k 1)))))

(insert-at 'alfa '(a b c d) 2) ; => (a alfa b c d)
