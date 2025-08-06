;; p17
(defun split (lst n)
  "Split LST into two parts at position N.
   Helper function for rotate."
  (let ((first '())
        (second lst)
        (count 0))
    (while (and second (< count n))
      (setq first (cons (car second) first))
      (setq second (cdr second))
      (setq count (1+ count)))
    (list (reverse first) second)))

(defun rotate (lst n)
  "Rotate LST N places to the left (positive N) or right (negative N)."
  (let* ((len (length lst))
         (effective-n (mod n len)))
    (if (zerop effective-n)
        lst
      (let ((split-result (split lst effective-n)))
        (append (cadr split-result) (car split-result))))))

(rotate '(a b c d e f g h) 3)   ; => (D E F G H A B C)
(rotate '(a b c d e f g h) -2)  ; => (G H A B C D E F)
