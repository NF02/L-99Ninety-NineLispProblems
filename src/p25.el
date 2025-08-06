;; Function to remove the K'th element from a list (1-based index)
(defun remove-at (lst k)
  (if (or (<= k 0) (> k (length lst)))
      lst
    (remove-at-helper lst 1 k)))

;; Helper function for remove-at
(defun remove-at-helper (lst current k)
  (cond ((null lst) nil)
        ((= current k) (remove-at-helper (cdr lst) (+ current 1) k))
        (t (cons (car lst) (remove-at-helper (cdr lst) (+ current 1) k)))))

;; Function to generate a random permutation of a list
(defun rnd-permu (lst)
  (if (null lst)
      nil
    (let* ((len (length lst))
           (pos (random len))
           (selected (nth pos lst))
           (remaining (remove-at lst (+ pos 1))))
      (cons selected (rnd-permu remaining)))))

(rnd-permu '(a b c d e f)) ; es. => (B A D C E F)
