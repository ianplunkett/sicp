(define (pascal n)
  (define (level n)
    (level-iter n 1))

  (define (level-iter n l)
    (if (< (- n l) 1)
	l 
	(level-iter
	 (- n l)
	 (+ l 1))))

  (define (remainder n)
    (remainder-iter n 1))

  (define (remainder-iter n l)
    (if (< (- n l) 1)
	(- l n)
	(remainder-iter
	 (- n l)
	 (+ l 1))))

  (define (level-position n)
    (- (level n) (remainder n)))

  (define (p n)
    (cond 
     ((> 1 n) 0)                              ; we don't deal with numbers less than one
     ((= 1 n) 1)                              ; if n  equals 1, return 1
     ((= 1 (level-position n)) 
      (p (- n (remainder n))))          ; if n is on a left edge, it has 1 parent
     ((= 0 (remainder n))                  
      (p (- n (level n))))            ; if n is on a right edge, it has 1 parent
     (else 
      (+ 
       (p (- n (level n))) 
       (p (+ (- n (level n)) 1))))))
  (p n))
