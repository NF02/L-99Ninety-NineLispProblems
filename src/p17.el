(defun split (lst n)
  "Split LST into two parts at position N. The first part contains N elements,
   the second contains the remainder."
  (let ((first '())
        (second lst)
        (count 0))
    ;; Build first part by taking elements from the front
    (while (and second (< count n))
      (setq first (cons (car second) first))
      (setq second (cdr second))
      (setq count (1+ count)))
    ;; Return both parts (reversing first to maintain order)
    (list (reverse first) second)))

(split '(a b c d e f g h i k) 3)  ; => ((A B C) (D E F G H I K))
