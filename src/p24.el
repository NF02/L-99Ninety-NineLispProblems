;; Function to generate a list of numbers from a to b
(defun range (a b)
  (cond ((= a b) (list a))
        ((< a b) (range-asc a b))
        (t (range-desc a b))))

;; Helper function for increasing range
(defun range-asc (a b)
  (if (> a b)
      nil
      (cons a (range-asc (+ a 1) b))))

;; Helper function for decreasing range
(defun range-desc (a b)
  (if (< a b)
      nil
      (cons a (range-desc (- a 1) b))))

;; Function to remove element at position k from list (1-based)
(defun remove-at (lst k)
  (if (or (<= k 0) (> k (length lst)))
      lst
      (remove-at-helper lst 1 k)))


;; Function to randomly select n elements from list;; Helper function for remove-at
(defun remove-at-helper (lst current k)
  (cond ((null lst) nil)
        ((= current k) (remove-at-helper (cdr lst) (+ current 1) k))
        (t (cons (car lst) (remove-at-helper (cdr lst) (+ current 1) k))))

(defun rnd-select (lst n)
  (if (or (<= n 0) (null lst))
      nil
      (let* ((len (length lst))
             (pos (random len))
             (selected (nth pos lst))
             (remaining (remove-at lst (+ pos 1))))
        (cons selected (rnd-select remaining (- n 1))))))

;; Main function to select n unique random numbers between 1 and m
(defun lotto-select (n m)
  (rnd-select (range 1 m) n))

(lotto-select 6 49) ;; es. => (26 9 25 18 33 17)
