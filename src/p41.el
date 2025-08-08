(defun is-prime (n &optional d)
  "Check if N is a prime number (recursive helper)."
  (cond ((<= n 1) nil)
        ((= (or d 2) n) t)
        ((zerop (mod n (or d 2))) nil)
        (t (is-prime n (1+ (or d 2))))))

(defun find-primes-up-to (n)
  "Return list of primes up to N."
  (let (primes)
    (dotimes (i (1+ n))
      (when (and (> i 1) (is-prime i))
        (push i primes)))
    (nreverse primes)))

(defun find-goldbach-pair (n primes)
  "Find Goldbach pair for N from list of PRIMES."
  (catch 'found
    (dolist (p primes)
      (when (is-prime (- n p))
        (throw 'found (list p (- n p)))))
    nil))

(defun goldbach-list (start end &optional min-prime)
  "Print Goldbach compositions for even numbers between START and END.
If MIN-PRIME is given, only show pairs where both primes >= MIN-PRIME."
  (let ((primes (find-primes-up-to end))
        results)
    (dolist (n (number-sequence start end))
      (when (and (>= n 2) (evenp n))
        (let ((pair (find-goldbach-pair n primes)))
          (when (or (null min-prime)
                    (and pair (>= (car pair) min-prime) (>= (cadr pair) min-prime)))
            (push (format "%d = %d + %d" n (car pair) (cadr pair)) results))))
    (mapc 'message (nreverse results)))))

;; Example usage:
(goldbach-list 9 20) ;; => nil
;; To find cases where both primes are >50 in range 2-3000:
(goldbach-list 2 3000 50) ;; => Args out of range: 36, 37
