#lang racket
(provide define-pda push-down pda epsilon)

(require rackunit)
(require racket/contract)
(require (for-syntax syntax/parse))

;; Macro for defining pda's to a name
(define-syntax (define-pda stx)
  (define-syntax-class start
    #:description "starting state"
    (pattern x:expr
        #:fail-unless (let ([thing (syntax->datum #'x)])
                        (and (list? thing) (= 2 (length thing))
                             (symbol? (cadr thing))))
        "Starting state must be a symbol"))
  (define-syntax-class trans
    #:description "transitions"
    (pattern x:expr
        #:fail-unless (let ([thing (syntax->datum #'x)])
                        (and (list? thing) (transitions? (cadr thing))))
        "Transitions must be a n x 5 2D list of symbols"))
  (define-syntax-class fin
    #:description "final states"
    (pattern x:expr
        #:fail-unless (let ([thing (syntax->datum #'x)])
                        (and (list? thing) (list? (cadr thing))
                             (andmap symbol? (cadr thing))))
        "Final states must be a list of symbols"))
    
  (syntax-parse stx
    [(_ name:id start:start transitions:trans finals:fin)
     #'(define name (list start (map (λ (x) (apply generate-trans x)) transitions)
                          finals))]))

;; Returns a pda given its start state, transitions, and final states
;; Symbol [List-of [List-of Symbol]] [List-of Symbol] -> Pda
(define (pda start transitions finals)
  (cond [(not (symbol? start))
         (raise-argument-error 'pda "symbol?" 0 start)]
        [(not (and (list? transitions) (andmap (λ (x) (and (list? x) (= 5 (length x))
                                                      (andmap symbol? x)))
                                          transitions)))
         (raise-argument-error 'pda "list of 5 symbol lists?" 1 transitions)]
        [(not (and (list? finals) (andmap symbol? finals)))
         (raise-argument-error 'pda "list of symbols?" 2 finals)]
        [else 
         (list start (map (λ (x) (apply generate-trans x)) transitions) finals)]))

;; Run the pda on the given input. Input can be formatted as a string or [List-of Symbol]
(define (push-down pda-des input)
  (let ([start (car pda-des)] [transitions (cadr pda-des)] [finals (caddr pda-des)])
    (if (not (and (symbol? start) (list? transitions) (andmap procedure? transitions)
                  (list? finals) (andmap symbol? finals)))
        (raise-argument-error 'push-down "pda?" pda-des)
         (pda-run (pda-c start input '()) transitions finals))))

;; Struct to represent a pda configuration. Holds current state, input, and stack
(struct pda-c (state input stack)
  #:guard (λ (state input stack name)
            (cond [(not (symbol? state)) (error name "state should be a symbol")]
                  [(not (or (and (list? input) (andmap symbol? input))
                            (string? input)))
                   (error name "input can be a list of symbols or a string")]
                  [(not (or (and (list? stack) (andmap symbol? stack))
                            (string? stack)))
                   (error name "stack can be a list of symbols or a string")]
                  [else (let* ([ch->sym (λ (x) (string->symbol (string x)))]
                               [string->symbols (λ (x) (map ch->sym (string->list x)))])
                          (values state (if (string? input) (string->symbols input) input)
                                  (if (string? stack) (string->symbols stack) stack)))])))



;; Generates a pda transition lambda that converts a configuration with the specified
;; symbol, input read, and stack contents to a new one
;; symbol char char symbol char -> procedurep
(define/contract (generate-trans initial-state input-read stack-top new-state new-stack)
  (-> symbol? symbol? symbol? symbol? symbol? procedure?)
  (λ (config) 
    (let ([ee? (λ (c) (symbol=? c epsilon))]
          [pda-input (pda-c-input config)]
          [pda-state (pda-c-state config)]
          [pda-stack (pda-c-stack config)])
      (cond [(or (not (symbol=? pda-state initial-state))
                 ;(empty? pda-input)
                 (not (or (ee? input-read) (and (not (empty? pda-input))
                                                (symbol=? input-read (car pda-input)))))
                 (and (not (ee? stack-top)) (
                                             empty? pda-stack))
                 (not (or (ee? stack-top) (ee? new-stack)
                          (symbol=? stack-top (car pda-stack))))
                 (and (not (or (ee? stack-top) (ee? new-stack))) (or (empty? pda-stack)
                                                (not (symbol=? stack-top (car pda-stack))))))
             #f]
            [(and (ee? input-read) (ee? new-stack) (ee? stack-top))
             (pda-c new-state pda-input pda-stack)]
            [(and (ee? input-read) (ee? new-stack))
             (pda-c new-state pda-input (cdr pda-stack))]
            [(and (ee? input-read) (ee? stack-top))
             (pda-c new-state pda-input (cons new-stack pda-stack))]
            [(ee? input-read)
             (pda-c new-state pda-input (cons new-stack (cdr pda-stack)))]
            [(and (ee? new-stack) (ee? stack-top))
             (pda-c new-state (cdr pda-input)
                  pda-stack)]
            [(ee? stack-top)
             (pda-c new-state (cdr pda-input)
                  (cons new-stack pda-stack))]
            [(ee? new-stack)
             (pda-c new-state (cdr pda-input)
                  (cdr pda-stack))]
            [else
             (pda-c new-state (cdr pda-input)
                  (cons new-stack (cdr pda-stack)))]))))

;; Runs through a PDA and returns true or false
(define (pda-run initial-config functions final-states)
  (letrec
      ([final-check
        (λ (states finals)
          (ormap (λ (state) (and (empty? (pda-c-input state)) (empty? (pda-c-stack state))
                                 (ormap (λ (x) (symbol=? x (pda-c-state state))) finals)))
                 states))]
       [apply-transitions
        (λ (states transitions)
          (flatten (map (λ (state)
                          (filter (λ (x) x) (map (λ (func) (func state)) transitions)))
                        states)))]
       [help
        (λ (states transitions finals)
          (let ([new-states (apply-transitions states transitions)])
            (cond [(final-check states finals) #t]
                  [(empty? new-states) #f]
                  [else (help new-states transitions finals)])))])
  (help (list initial-config) functions final-states)))


;; Helper method to check for [List-of [List-of Symbol]
(define-for-syntax (transitions? t)
  (and (list? t) (andmap (λ (x) (and (list? x) (= 5 (length x))
                                     (andmap symbol? x))) t)))

;; Is this pda-config equal to that pda-config?
;; pda-config pda-config -> boolean
(define (config=? config1 config2)
  (letrec ([charl=? (λ (l lc)
                      (cond [(and (empty? l) (empty? lc)) #t]
                            [(or (empty? l) (empty? lc) (not (symbol=? (car l) (car lc)))) #f]
                            [else (charl=? (cdr l) (cdr lc))]))])
    (and (pda-c? config1) (pda-c? config2)
         (symbol=? (pda-c-state config1) (pda-c-state config2))
         (charl=? (pda-c-input config1) (pda-c-input config2))
         (charl=? (pda-c-stack config1) (pda-c-stack config2)))))


;; Self-explanatory
(define epsilon 'ɛ)

;  ===================================TESTS====================================

(define config1 (pda-c 's "abba" '()))
(define config2 (pda-c 's "bba" "a"))
(define config3 (pda-c 's "bba" '(a)))
(define config4 (pda-c 'f "" "asdfj"))

(check-true (config=? config2 config3))
(check-false (config=? config1 config4))
(check-true (config=? config4 config4))

(check-pred procedure? (generate-trans 'a epsilon 'a 'f 'S))

(check-true (config=? ((generate-trans 's epsilon epsilon 'f epsilon) (pda-c 's '() '()))
                      (pda-c 'f '() '())))
(check-false ((generate-trans 's 'a 'b 'f 'a) config1))
(check-false ((generate-trans 's 'b epsilon 'f epsilon) config1))
(check-false ((generate-trans 's 'a 'b 'f epsilon) config1))

(check-true (config=? ((generate-trans 's 'b 'a 'f epsilon) (pda-c 's "b" '(a)))
                      (pda-c 'f "" '())))

(check-true (pda-run (pda-c 's "b" '(a))
                     (list (generate-trans 's 'b epsilon 'f epsilon)
                           (generate-trans 'f 'ɛ 'a 'f 'ɛ))
                     '(f)))
(check-true (pda-run config1
                     (list
                      (generate-trans 's 'a epsilon 'f epsilon)
                      (generate-trans 'f 'a epsilon 'f epsilon)
                      (generate-trans 'f 'b epsilon 'f epsilon))
                     '(f )))

(check-true (config=? config2 ((generate-trans 's 'a epsilon 's 'a) config1)))
(check-true (config=? (pda-c 's "ba" "") ((generate-trans 's 'b 'a 's epsilon)
                                        config2)))
(check-true (config=? (pda-c 'f "" '()) ((generate-trans 's 'a 'a 'f epsilon)
                                       (pda-c 's "a" "a"))))
(define-pda pda-1 's
  '((s a ɛ s a)
    (s b ɛ s b)
    (s ɛ ɛ f ɛ)
    (f a a f ɛ)
    (f b b f ɛ))
  '(f))
(define-pda pda-2 's
  '((s a ɛ s a)
    (s b ɛ s b)
    (s ɛ ɛ f ɛ)
    (f a a f ɛ)
    (f b b f ɛ))
  '(f))
(define-pda pda-3 'sp
  '((sp ɛ ɛ s l)
    (s a ɛ s a)
    (s b ɛ s b)
    (s ɛ ɛ f ɛ)
    (f a a f ɛ)
    (f b b f ɛ)
    (f ɛ l d ɛ))
  '(d))

(check-true (push-down pda-1 '(a b b a)))
(check-false (push-down pda-2 '(a b b a c)))
(check-false (push-down pda-3 '(a b b a a)))

(check-true (push-down (pda 's
  '((s a ɛ s a)
    (s b ɛ s b)
    (s ɛ ɛ f ɛ)
    (f a a f ɛ)
    (f b b f ɛ))
  '(f)) "abba"))
(check-false (push-down (pda 's
  '((s a ɛ s a)
    (s b ɛ s b)
    (s ɛ ɛ f ɛ)
    (f a a f ɛ)
    (f b b f ɛ))
  '(f)) "abbac"))
(check-false (push-down (pda 'sp
  '((sp ɛ ɛ s l)
    (s a ɛ s a)
    (s b ɛ s b)
    (s ɛ ɛ f ɛ)
    (f a a f ɛ)
    (f b b f ɛ)
    (f ɛ l d ɛ))
  '(d)) "abbaa"))