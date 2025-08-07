(defun is-prime (n &optional divisor)
  "Recursive prime check with optional divisor argument."
  (let ((divisor (or divisor 2)))  ; Default divisor is 2
  (cond
   ((<= n 1) nil)
   ((= n 2) t)
   ((> (* divisor divisor) n) t)
   ((zerop (mod n divisor)) nil)
   (t (is-prime n (if (= divisor 2) 3 (+ divisor 2)))))))

;; example usage
(is-prime 7) ;; => t
(is-prime 8) ;; => nil
