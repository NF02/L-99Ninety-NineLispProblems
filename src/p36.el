(defun prime-factors-mult (n)
  "Determine the prime factors of a given positive integer (2)."
  (defun factor-helper (n divisor count factors)
    (cond ((= n 1) (if (zerop count) factors (cons (list divisor count) factors)))
          ((zerop (mod n divisor)) (factor-helper (/ n divisor) divisor (1+ count) factors))
          (t (factor-helper n (1+ divisor) 0 (if (zerop count) factors (cons (list divisor count) factors))))))
  (reverse (factor-helper n 2 0 '())))

;; example usage
(prime-factors-mult 315) ;; =>  ((3 2) (5 1) (7 1))
