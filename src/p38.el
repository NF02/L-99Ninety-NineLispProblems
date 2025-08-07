(defun gcd (a b)
  (if (= b 0) a (gcd b (% a b))))

(defun coprime (a b)
  (= (gcd a b) 1))

(defun totient-phi (m &optional r count)
  "Count numbers from 1 to m-1 coprime with m."
  (let ((r (or r 1))             ; Start from 1
        (count (or count 0)))     ; Initialize count
    (cond
     ((= m 1) 1)                 ; φ(1) = 1
     ((>= r m) count)            ; Base case: finished
     ((coprime r m)              ; If r is coprime with m
      (totient-phi m (1+ r) (1+ count))) ; Increment count
     (t                          ; If not coprime
      (totient-phi m (1+ r) count)))))   ; Keep count unchanged

(defun phi-improved (prime-factors)
  (if (null prime-factors)
      1
    (let* ((pair (car prime-factors))
           (p (car pair))
           (m (cadr pair)))
      (* (1- p)
         (expt p (1- m))
         (phi-improved (cdr prime-factors))))))

;; Example usage
(phi-improved '((2 1) (5 1) (1009 1))) ;; => (2−1)×2^0×(5−1)×5^0×(1009−1)×1009^0=4032
