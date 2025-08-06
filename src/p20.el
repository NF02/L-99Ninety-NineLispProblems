(defun remove-at (lst k)
  (if (or (<= k 0) (> k (length lst)))
      lst
      (remove-at-helper lst 1 k)))

(defun remove-at-helper (lst current k)
  (cond ((null lst) nil)
        ((= current k) (remove-at-helper (cdr lst) (+ current 1) k))
        (t (cons (car lst) (remove-at-helper (cdr lst) (+ current 1) k)))))

(remove-at '(a b c d) 2) ; => (a c d)
