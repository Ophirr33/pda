#lang racket
(provide define-pda pda epsilon)
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
     #'(define name (λ (input)
                      (push-down (list start (map (λ (x) (apply generate-trans x)) transitions)
                                       finals)
                                 input)))]))

;; Returns a pda given its start state, transitions, and final states
;; Symbol [List-of [List-of Symbol]] [List-of Symbol] -> ([List-of Symbol] -> Boolean)
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
         (λ (input) (push-down
                     (list start (map (λ (x) (apply generate-trans x))
                                      transitions) finals)
                     input))]))

;; Run the pda on the given input. Input can be formatted as a string or [List-of Symbol]
(define (push-down pda-des input)
  (let ([start (car pda-des)] [transitions (cadr pda-des)] [finals (caddr pda-des)])
    (cond [(not (and (symbol? start) (list? transitions) (andmap procedure? transitions)
                     (list? finals) (andmap symbol? finals)))
           (raise-argument-error 'push-down "pda?" pda-des)]
          [(not (or (string? input) (and (list? input) (andmap symbol? input))))
           (raise-argument-error 'push-down "string? or list-of symbol?" input)]
          [else (pda-run (pda-c start input '()) transitions finals)])))

;; Struct to represent a pda configuration. Holds current state, input, and stack
(struct pda-c (state input stack)
  #:guard (λ (state input stack name)
            (let* ([ch->sym (λ (x) (string->symbol (string x)))]
                   [string->symbols (λ (x) (map ch->sym (string->list x)))])
              (values state (if (string? input) (string->symbols input) input)
                      (if (string? stack) (string->symbols stack) stack)))))



;; Generates a pda transition lambda that converts a configuration with the specified
;; symbol, input read, and stack contents to a new one
;; symbol char char symbol char -> procedurep
(define (generate-trans initial-state input-read stack-top new-state new-stack)
  (λ (config) 
    (let ([ee? (λ (c) (symbol=? c epsilon))]
          [pda-input (pda-c-input config)]
          [pda-state (pda-c-state config)]
          [pda-stack (pda-c-stack config)])
      (cond [(or (not (symbol=? pda-state initial-state))
                 (not (or (ee? input-read) (and (not (empty? pda-input))
                                                (or (symbol=? input-read '__)
                                                    (symbol=? input-read (car pda-input))))))
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
