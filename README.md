# pda
Push Down Automaton implementation in Racket

## Setup
In DrRacket, go to Install Package and paste in "https://github.com/Ophirr33/pda.git"

Or

git clone https://github.com/Ophirr33/pda.git

raco pkg install --link pda

Now just `(require pda)` at the top of the file and you're good to go!

## Usage
The four functions available to you from this module are
- define-pda
- pda
- push-down
- epsilon

This module allows you to create pdas from their start state, list of transitions, and list of final states.
Then, one can run the defined PDA on different inputs using push-down. Input can be specified as a string,
which is converted to a list of single character symbols, or as a list of symbols. Epsilon is simply a 
shortcut to write 'ɛ, the special symbol that specifies an empty input or stack transition. pda is a function
which will create a pda without any name bindings and slightly different error reporting, but functionally
behaves the same. The PDA will return #t for accepting inputs, and #f for rejecting inputs.

For example, the following code defines a PDA that accepts the reverse of strings made of "a" or "b".
```
(define-pda pda-1 's
  '((s a ɛ s a)
    (s b ɛ s b)
    (s ɛ ɛ f ɛ)
    (f a a f ɛ)
    (f b b f ɛ))
  '(f))
  
  (push-down pda-1 '(a b b a))  ;; --> #t
  (push-down pda-1 "baab"))     ;; --> #t
```
and the following accepts the kleene-star + 1 of "ab"
```
(push-down (pda 's
  '((s ɛ ɛ f S)
    (f ɛ S c S)
    (c ɛ ɛ f S)
    (f ɛ S d b)
    (d ɛ ɛ f a)
    (f a a f ɛ)
    (f b b f ɛ)) '(f)))         ;; --> #t
```

### Ideas for improvement
- Spot any infinitely looping setups and just return #f instead
- Allow for multi-stack transitions (basically allow for kleene star transition functions in case of new-stack?)
