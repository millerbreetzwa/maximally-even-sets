;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname MESets) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/image)
(require racket/list)

(define NOTE-SIZE 4)
(define C-NOTE-COLOR 'black)
(define D-NOTE-COLOR 'red)
(define C-NOTE-DOT (circle NOTE-SIZE 'solid C-NOTE-COLOR))
(define D-NOTE-DOT (circle NOTE-SIZE 'solid D-NOTE-COLOR))
(define LENGTH 200)
(define BACKGROUND (empty-scene (+ 5 LENGTH) (+ 5 LENGTH)))
(define PC-CIRCLE (circle (/ LENGTH 2) 'outline 'black))

;; An Exploration of Maximally Even Scales
;; Walker Miller-Breetz 2017

;; A Natural is one of
;; 0
;; (add1 Nat)
;; Interpretation: the natural numbers 0,1,2,...

;; A Scale is a (list Natural [List-of Natural])
;; Interpretation: The first number is c, the total number of pitches in the chromatic universe
;;   while the inner list represents the d pitches in the scale (the diatonic universe)

;; Examples of Scales:
(define WHOLE-TONE (list 12 (list 0 2 4 6 8 10))) ;; ME
(define OCTATONIC (list 12 (list 0 1 3 4 6 7 9 10))) ;; ME
(define MAJOR1 (list 12 (list 0 2 4 5 7 9 11))) ;; ME
(define MAJOR2 (list 12 (list 2 4 6 7 9 11 1))) ;; ME
(define HARMONIC-MINOR (list 12 (list 0 2 3 5 7 9 11))) ;; not ME
(define 15-5-SCALE-1 (list 15 (list 0 3 6 9 12))) ;; ME
(define 15-5-SCALE-2 (list 15 (list 1 4 7 10 13))) ;; ME
(define 15-5-SCALE-3 (list 15 (list 0 2 5 8 10))) ;; not ME
(define 15-8-SCALE-1 (list 15 (list 0 1 3 5 7 9 11 13))) ;; ME
(define 15-8-SCALE-2 (list 15 (list 0 2 3 5 7 9 11 13))) ;; ME

;; scale=?: Scale Scale -> Boolean
;; Purpose: Tells if two scales are equal (two scales might have the same list of notes but permuted)
(check-expect (scale=? MAJOR1 MAJOR1) #true)
(check-expect (scale=? MAJOR1 MAJOR2) #false)
(check-expect (scale=? (list 5 (list 0 2 4)) (list 5 (list 2 0 4))) #true)
(check-expect (scale=? (list 5 (list 0 2 4)) (list 6 (list 2 0 4))) #false)

(define (scale=? s1 s2)
  (and (= (first s1) (first s2))
       (equal? (sort (second s1) <) (sort (second s2) <))))

;; transpose: Scale Natural -> Scale
;; Purpose: Transposes a scale by the given natural
(check-expect (transpose MAJOR1 2) MAJOR2)
(check-expect (transpose 15-5-SCALE-1 1) 15-5-SCALE-2)

(define (transpose scale n)
  (list (first scale) (map (λ (x) (modulo (+ x n) (first scale))) (second scale))))

;; clen: Natural Natural Scale -> Natural
;; Purpose: Returns the clen of the interval given by the ith and jth elements of the given scale
(check-expect (clen 0 1 WHOLE-TONE) 2)
(check-expect (clen 3 6 OCTATONIC) 5)
(check-expect (clen 3 1 15-5-SCALE-1) 9)

(define (clen i j scale)
  (modulo (- (list-ref (second scale) j) (list-ref (second scale) i)) (first scale)))

;; dlen: Natural Natural Scale -> Natural
;; Purpose: Returns the dlen of the interval given by the ith and jth elements of the given scale
(check-expect (dlen 0 1 WHOLE-TONE) 1)
(check-expect (dlen 3 6 OCTATONIC) 3)
(check-expect (dlen 3 1 15-5-SCALE-1) 3)

(define (dlen i j scale)
  (modulo (- j i) (length (second scale))))

;; spectrum: Natural Scale -> [List-of Natural]
;; Purpose: Returns the list of clens corresponding to a dlen in a given scale
(check-expect (spectrum 2 MAJOR1) (list 3 4))
(check-expect (spectrum 3 MAJOR1) (list 5 6))
(check-expect (spectrum 3 HARMONIC-MINOR) (list 4 5 6))
(check-expect (spectrum 1 15-5-SCALE-1) (list 3))

(define (spectrum dlen scale)
  (local ((define extended-scale (list (first scale) (append (second scale)
                                                             (take (second scale) dlen))))

          ;; spectrum/a: Natural Scale [List-of Natural] -> [List-of Natural]
          ;; Purpose: Returns the list of clens corresponding to a dlen in a given scale
          ;; ACCUM: acc-clens stores all the clens of each dlen
          (define (spectrum/a dlen scale acc-clens)
            (cond
              [(<= (length (second scale)) dlen) acc-clens]
              [else (spectrum/a dlen (list (first scale) (rest (second scale)))
                                (cons (clen 0 dlen scale) acc-clens))])))

    (sort (remove-duplicates (spectrum/a dlen extended-scale empty)) <)))

;; maximally-even?: Scale -> Boolean
;; Purpose: Tells if a given scale is maximally even
(check-expect (maximally-even? WHOLE-TONE) #true)
(check-expect (maximally-even? OCTATONIC) #true)
(check-expect (maximally-even? MAJOR1) #true)
(check-expect (maximally-even? HARMONIC-MINOR) #false)
(check-expect (maximally-even? 15-5-SCALE-1) #true)
(check-expect (maximally-even? 15-5-SCALE-2) #true)
(check-expect (maximally-even? 15-5-SCALE-3) #false)
(check-expect (maximally-even? 15-8-SCALE-1) #true)

(define (maximally-even? scale)
  (andmap (λ (spectrum-length) (or (= spectrum-length 1) (= spectrum-length 2)))
          (map length (build-list (length (second scale)) (λ (dlen) (spectrum dlen scale))))))

;; create-me: Natural Natural Natural -> Scale
;; Purpose: Creates a maximally even scale with d notes in a chromatic universe of c notes
;;  with a transposition factor of m
(check-expect (create-me 12 7 5) MAJOR1)
(check-expect (create-me 12 8 0) OCTATONIC)
(check-expect (create-me 12 6 0) WHOLE-TONE)
(check-expect (create-me 15 5 0) 15-5-SCALE-1)
(check-expect (create-me 15 5 5) 15-5-SCALE-2)

(define (create-me c d m)
  (list c (build-list d (λ (x) (floor (/ (+ (* x c) m) d))))))

;; list-all-me: Natural Natural -> [List-of Scales]
;; Purpose: Lists all maximally even sets with the given parameters (there will be c/gcd(c,d) of them)
(check-expect (list-all-me 12 8)
              (list (list 12 (list 0 1 3 4 6 7 9 10))
                    (list 12 (list 0 2 3 5 6 8 9 11))
                    (list 12 (list 1 2 4 5 7 8 10 11))))
(check-expect (list-all-me 12 6)
              (list (list 12 (list 0 2 4 6 8 10))
                    (list 12 (list 1 3 5 7 9 11))))
(check-expect (list-all-me 12 7)
              (list (list 12 (list 0 1 3 5 6 8 10))
                    (list 12 (list 0 1 3 5 7 8 10))
                    (list 12 (list 0 2 3 5 7 8 10))
                    (list 12 (list 0 2 3 5 7 9 10))
                    (list 12 (list 0 2 4 5 7 9 10))
                    (list 12 (list 0 2 4 5 7 9 11))
                    (list 12 (list 0 2 4 6 7 9 11))
                    (list 12 (list 1 2 4 6 7 9 11))
                    (list 12 (list 1 2 4 6 8 9 11))
                    (list 12 (list 1 3 4 6 8 9 11))
                    (list 12 (list 1 3 4 6 8 10 11))
                    (list 12 (list 1 3 5 6 8 10 11))))

(define (list-all-me c d)
  (remove-duplicates (build-list c (λ (m) (create-me c d m)))))

;; draw-scale: Scale -> Image
;; Purpose: Draws the given scale around the pitch class circle

(define (draw-scale scale)
  (local ((define list-of-angles (build-list (first scale)
                                             (λ(x) (- (/ (* x 2 pi) (first scale)) (/ pi 2)))))

          (define list-of-posns (map (λ (p) (make-posn (+ (posn-x p) (/ LENGTH 2))
                                                       (+ (posn-y p) (/ LENGTH 2))))
                                     (map (λ (a) (make-posn (* (- (/ LENGTH 2) 2) (cos a))
                                                            (* (- (/ LENGTH 2) 2) (sin a))))
                                          list-of-angles)))

          (define (draw-d-note posn img)
            (place-image D-NOTE-DOT (posn-x posn) (posn-y posn) img))

          (define (draw-c-note posn img)
            (place-image C-NOTE-DOT (posn-x posn) (posn-y posn) img)))
    (foldr (λ (p i) (if (member? (index-of list-of-posns p) (second scale))
                       (draw-d-note p i)
                       (draw-c-note p i)))
           PC-CIRCLE list-of-posns)))

;; draw-all-me: Natural Natural -> [List-of Images]
;; Draws all maximally even scales with the given parameters

(define (draw-all-me c d)
  (map draw-scale (list-all-me c d)))