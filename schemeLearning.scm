; Hello world as a variable
(define vhello "Hello world") ;
; Hello world as a function
(define fhello (lambda () "Hello world")) ;
; Hello with name
(define Hello
    (lambda (name) 
        (string-append "Hello " name "!"))) ;
; Sum of three numbers
(define sum3
    (lambda (a b c) 
        (+ a b c))) ;            
; hello with name 2
(define (hello2 name)
    (string-append "Hello " name "!")) ;
; sum of three numbers 2
(define (sum32 a b c)
    (+ a b c)) ;
                    

; test 4-2-1
(define pi (* 4 (atan 1.0)))
(define (rotate2arc rotate)
    (/ (* rotate pi) 180));

; test 4-2-2
(define (getL v t)
    (* v t)) ;

; test 4-2-3
(define g 9.8) ;
(define (getT vy)
    (/ (* 2 vy) g)) ;

; test 4-2-4
(define (getVX v0 theta)
    (* v0 
        (cos theta))) ;
    
(define (getVY v0 theta)
    (* v0 
        (sin theta))) ;

(define (getLX v0 theta)
    (* (getVX v0 theta) 
        (getT (getVY v0 theta)))) ;

; test 4-2-5
(define answer 
    (getLX 40 (rotate2arc 30)))

; test 5-1-1
(define (getAbs num)
    (if (> num 0)
        num (- num))) ;
; test 5-1-2
(define (getRever num)
    (if (= num 0)
        #f (/ 1 num))) ;
; test 5-1-3
(define (getASC num)
    (if (<= 33 num 126) 
        (integer->char num) #f)) ;

; test 5-2-1
(define (getThreeX+ a b c)
    (and (positive? a) (positive? b) (positive? c) (* a b c))) ;
; test 5-2-2
(define (getThreeX- a b c) 
    (if (or (negative? a) (negative? b) (negative? c)) 
        (* a b c) #f)) ;

; test 5-2-3
(define (mapNum num) 
    (cond 
        ((>= num 80) "A")
        ((<= 60 num 79) "B")
        ((<= 40 num 59) "C")
        ((< 40) "D"))) ;


; test 7-1-1 (my-length '(1 2 3))
(define (my-length ls)
    (if (null? ls) 
        0
        (+ 1 (my-length (cdr ls)))));

;test 7-1-2 (sum-list '(1 2 3))
(define (sum-list ls)
    (if (null? ls)
        1
        (+ (car ls) (sum-list (cdr ls))))) ;

;test 7-1-3 (del-list 2 '(1 2 3) )
(define (del-list obj ls)
    (if (null? ls)
        '()
        (let ((h (car ls)))
            ((if (eqv? obj h)
                (lambda (y) y)
                (lambda (y) (cons h y)))
            (del-list obj (cdr ls))
            )
        )
    )
)

;test 7-1-4 (getIndex-list '(1 2 3) 2) =>1
(define (getIndex-list ls x) 
    (find-index ls x 0)
)
(define (find-index ls x length)
    (cond
        ((null? ls) #f)
        ((eqv? x (car ls)) length)
        (else (find-index (cdr ls) x (1+ length) ))
    )
    ; (if (null? ls)
    ;     #f    
    ;     (let ((item (car ls)))
    ;         (if (eqv? item x)
    ;             length
    ;             (find-index (cdr ls) x (1+ length))
    ;         )
    ;     )
    ; )
)

;test 7-2-1 (my-reverse '(1 2 3)) => '(3 2 1)
(define (my-reverse ls )
    (my-reverse-rec ls '())
)
(define (my-reverse-rec ls new-ls)
    (if (null? ls)
        new-ls
        (my-reverse-rec (cdr ls) (cons (car ls) new-ls ))
    )
)
;test 7-2-2 (sum-list2 '(1 2 3)) => 6
(define (sum-list2 ls)
    (sum-list2-rec ls 0)
)
(define (sum-list2-rec ls sum)
    (if(null? ls)
        sum
        (sum-list2-rec (cdr ls) (+ (car ls) sum))
    )
)
;test 7-2-3 (get-num "123") => 123
(define (get-num str)
    (get-num-rec (string->list str) 0)
)
(define (char->num char)
    (- (char->integer char) 48)
)
(define (get-num-rec ls sum)
    (if (null? ls)
        sum
        (get-num-rec (cdr ls) (+ (* 10 sum) (char->num (car ls))))
    )
)

;test 7-3-1 (loop-del-list 2 '(1 2 3) ) => '(1 3)
(define (loop-del-list x ls)
    (let loop((inner-ls ls) (res '()))
        (if (null? inner-ls)
            (reverse res)
            (let ((item (car inner-ls)))
                (loop (cdr inner-ls)  
                    (if (eqv? x item)
                        res
                        (cons item res)
                    )
                )
            )
        )
    )
)
;test 7-3-2 (loop-sum-list '(1 2 3) ) => 6
(define (loop-sum-list ls)
    (let loop((inner-ls ls) (sum 0))
        (if (null? inner-ls)
            sum
            (loop (cdr inner-ls)  
                (+ sum (car inner-ls))
            )
            
        )
    )
)

;test 7-3-3 (loop-my-reverse '(1 2 3)) => '(3 2 1)
(define (loop-my-reverse ls)
    (let loop((inner-ls ls) (res '()))
        (if (null? inner-ls)
            res
            (loop (cdr inner-ls) (cons (car inner-ls) res))
        )
    )
)

;test 7-3-4 (loop-sum-list2 '(1 2 3)) => 6
;test 7-3-5 (loop-get-num "123") => 123
(define (loop-get-num str)
    (let loop((inner-ls (string->list str)) (num 0))
        (if (null? inner-ls) 
            num
            (loop (cdr inner-ls) (+ (* num 10) (char->num (car inner-ls))))    
        )
    )
)
;test 7-4-1 (letrec-my-reverse '(1 2 3)) => '(3 2 1)
(define (letrec-my-reverse ls)
    (letrec 
        ((func 
            (lambda (inner-ls res)
                (if (null? inner-ls)
                    res
                    (func (cdr inner-ls) (cons (car inner-ls) res))                
                )
            )
        ))
        (func ls '())
    )
)

;test 7-4-1 (do-my-reverse '(1 2 3)) => '(3 2 1)
(define (do-my-reverse ls)
    (do (
            (inner-ls ls (cdr inner-ls)) 
            (res '() (cons (car inner-ls) res))
        )
        ((null? inner-ls) res)
    )
)

;test 8-1-1 (map-double '(1 2 3)) => '(2 4 6)
(define (map-double ls)
    (map
        (lambda (x) (* 2 x) )
        ls
    )
)

;test 8-1-2 (map-reducer '(1 2 3) '(4 4 4)) => '(-3 -2 -1)
(define (map-reducer ls ls2)
    (map
        ; (lambda (x x2) (- x x2))
        -
        ls
        ls2    
    )
)

; define filter
(define (filter func ls)
    (let ((new-ls '()))
        (for-each 
            (lambda (x) 
                (if (func x)
                   (set! new-ls (cons x new-ls))
                   (set! new-ls  new-ls)
                )
            ) 
            ls
        )
        (reverse new-ls)
    )
)
;test 8-2-1 (filter-even '(1 2 3 4)) => '(2 4)
(define (filter-even ls)
    (filter  (lambda (x) (eqv? 0 (modulo x 2))) ls)
)
;test 8-2-2 (filter-two-digit '(1 11 111 2 22 222 3 33 333)) => '(1 111 2 222 3 333)
(define (filter-two-digit ls)
    (filter  (lambda (x) 
            (or (< x 10) (> x 100))
        ) 
        ls
    )
)

;test 8-3-1 (reduce-squaring-sum-sqrt '(3 4)) => 5
(define (reduce-squaring-sum-sqrt ls)
    (sqrt 
        (fold-left (lambda (x1 x2) 
            (+ x1 (* x2 x2)))
        0 ls)
    )
)

;test 8-4-1 (sort-sin '(80 90 101)) => '(80 101 90)
(define (sort-sin ls)
    (sort  (lambda (x1 x2) (< (sin x1) (sin x2))) ls)
)

;test 8-4-2 (sort-length '((2 3 4) (1 2 3 4) (3 4)) ) => '((1 2 3 4) (2 3 4) (3 4))
(define (sort-length ls)
  (sort (lambda (x y) (> (length x) (length y))) ls)
)

;test 8-5-1 (apply-squaring-sum-sqrt '(3 4)) => 5
(define (apply-squaring-sum-sqrt ls)
    (sqrt 
        (apply + (map (lambda (x) (+ (* x x))) ls))
    )
)

;test 11-1-1 (title-style "the cathedral and the bazaar") ⇒ "The Cathedral And The Bazaar"
(define (title-style str) 
    (list->string 
        (reverse   
            (fold-left 
                (lambda (res x)
                    (if (null? res)
                        (cons (char-upcase x) res)
                        (if (eqv? #\space (car res))
                            (cons (char-upcase x) res)
                            (cons x res)
                        )
                    )
                )
                '()
                (string->list str)
            )
        )
    )
)