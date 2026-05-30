;; SPDX-FileCopyrightText: 2026 Artyom Bologov
;; SPDX-License-Identifier: MIT

;;; Permission is hereby granted, free of charge, to any person
;;; obtaining a copy of this software and associated documentation
;;; files (the "Software"), to deal in the Software without
;;; restriction, including without limitation the rights to use,
;;; copy, modify, merge, publish, distribute, sublicense, and/or
;;; sell copies of the Software, and to permit persons to whom the
;;; Software is furnished to do so, subject to the following
;;; conditions:
;;;
;;; The above copyright notice and this permission notice shall be
;;; included in all copies or substantial portions of the Software.
;;;
;;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
;;; EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES
;;; OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
;;; NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT
;;; HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY,
;;; WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
;;; FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR
;;; OTHER DEALINGS IN THE SOFTWARE.

(cond-expand
  (kawa
   (define (get-arg-num proc :: procedure)
     (let* ((numArgs (proc:numArgs))
            (rest? (negative? numArgs)))
       (values (bitwise-and numArgs #b11111111111) rest?)))

   (define (get-arg-types proc :: procedure)
     (let-values (((num rest?) (get-arg-num proc)))
       (let rec ((param 0))
         (cond
          ((and (= param num) rest?)
           (proc:getParameterType param))
          ((= param num)
           '())
          (else
           (cons (proc:getParameterType param) (rec (+ 1 param))))))))

   (define (get-check type)
     (let* ((name ((type:toString):replace "ClassType " ""))
            (name (name:replace "Type " "")))
       ;; TODO: Parameters
       (cond
        ((equal? name "java.lang.Object") 'check-any?)
        ((equal? name "gnu.mapping.Symbol") 'symbol?)
        ((equal? name "gnu.expr.Keyword") 'keyword?)
        ((equal? name "list") 'list?)
        ;; TODO: Pair?
        ((equal? name "java.lang.CharSequence") 'string?)
        ((equal? name "character") 'character?)
        ((equal? name "vector") 'vector?)
        ((equal? name "gnu.mapping.Procedure") 'procedure?)
        ((equal? name "java.io.Reader") 'input-port?)
        ((equal? name "java.io.Writer") 'input-port?)
        ((equal? name "gnu.lists.Array") 'array?)
        ((equal? name "java.lang.Number") 'number?)
        ((equal? name "java.io.Closeable") 'port?)
        ((equal? name "gnu.math.Complex") 'complex?)
        ((equal? name "gnu.math.Quantity") 'quantity?)
        ((member name '("real" "rational" "integer"
                        "long" "int" "short" "byte"
                        "ulong" "uint" "ushort" "ubyte"
                        "double" "float"))
         (string->symbol (string-append name "?")))
        (else name))))

   (define (get-arg-checks proc :: procedure)
     (case (if proc:name
               (string->symbol proc:name)
               #f)
       ((+ * - /) 'complex?)
       ((apply)
        '(procedure? . check-any?))
       ((array-ref)
        '(array? . integer?))
       ((array-set!)
        '(array? . check-any?))
       ((bitwise-and bitwise-ior bitwise-xor)
        'integer?)
       ((bitwise-arithmetic-shift bitwise-arithmetic-shift-left bitwise-arithmetic-shift-right)
        '(integer? integer?))
       ((bitwise-not) '(integer?))
       ((call-with-current-continuation call/cc)
        '(procedure?))
       ((call-with-values)
        '(procedure? procedure?))
       ((format)
        '((check-or? boolean? string? number? output-port?)
          . check-any?))
       ((floor/ floor-quotient floor-remainder
                truncate/ truncate-quotient truncate-remainder
                quotinent remainder
                div mod modulo div0 mod0)
        '(integer? integer?))
       ((expt)
        '(complex? complex?))
       ((eq? eqv? equal?)
        '(check-any? check-any?))
       ((list)
        'check-any?)
       ((make-procedure)
        'check-any?)
       ((map for-each)
        '(procedure? list? . list?))
       ((> = < >= <=)
        '(number? number? . number?))
       ((run-process)
        'check-any?)
       ((even? odd?)
        '(integer?))
       (else
        (let ((types (get-arg-types proc)))
          (let rec ((types types))
            (cond
             ((pair? types)
              (cons (get-check (car types))
                    (rec (cdr types))))
             ((null? types)
              '())
             (else (get-check types))))))))

   (define (get-procedure-check proc :: procedure)
     (list 'check-procedure-of
           (cons 'list (get-arg-checks proc))
           ;; Kawa does not retain return types???
           'check-any?))))

(define-syntax define-check
  (syntax-rules ()
    ((_ name predicate)
     (define name predicate))))

(define (derive-check thing)
  #f)

(cond-expand
  (chicken
   ;; TODO: Chicken has:
   ;; - union types
   ;; - not types
   ;; - pair, list-of, and vector-of
   ;; - multiple value return types
   ;;
   ;; It would be nice to implement all of these, but it’d require a
   ;; lot of human-hours and non-hygienic macros. Yuck!
   (define-syntax %declare-checked-var
     (syntax-rules (: ->
                      integer? exact-integer? boolean? char? complex?
                      fixnum? flonum?
                      eof? inexact? real?
                      list? null? number? pair?
                      input-port? output-port?
                      procedure? rational?
                      string? symbol? keyword? vector? pointer?
                      check-any? check-list-of? check-vector-of? check-pair-of? check-procedure-of?

                      integer boolean char cplxnum eof fixnum float
                      number list null number pair input-port output-port
                      procedure ratnum string symbol keyword vector blob pointer *)
       ((_ name any?)           (: name *))
       ((_ name fixnum?)        (: name fixnum))
       ((_ name flonum?)        (: name float))
       ((_ name integer?)       (: name number))
       ((_ name exact-integer?) (: name integer))
       ((_ name boolean?)       (: name boolean))
       ((_ name char?)          (: name char))
       ((_ name complex?)       (: name cplxnum))
       ((_ name eof?)           (: name eof))
       ((_ name inexact?)       (: name float))
       ((_ name real?)          (: name number))
       ((_ name list?)          (: name list))
       ((_ name (check-list-of? _)) (: name list))
       ((_ name (check-list-of? _ _)) (: name list))
       ((_ name null?)          (: name null))
       ((_ name number?)        (: name number))
       ((_ name pair?)          (: name pair))
       ((_ name (check-pair-of? _ _)) (: name pair))
       ((_ name input-port?)    (: name input-port))
       ((_ name output-port?)   (: name output-port))
       ((_ name procedure?)     (: name procedure))
       ((_ name (check-procedure-of? _ _)) (: name procedure))
       ((_ name rational?)      (: name ratnum))
       ((_ name string?)        (: name string))
       ((_ name symbol?)        (: name symbol))
       ((_ name keyword?)       (: name keyword))
       ((_ name vector?)        (: name vector))
       ((_ name (check-vector-of? _)) (: name vector))
       ((_ name pointer?)       (: name pointer))
       ((_ name predicate)
        (when #f #f))))
   (define-syntax %declare-checked-fn/return
     (syntax-rules (: ->
                      any? integer? boolean? char? complex?
                      fixnum? flonum?
                      eof? inexact? real?
                      list? null? number? pair?
                      input-port? output-port?
                      procedure? rational?
                      string? symbol? keyword? vector? pointer?
                      check-any? check-list-of? check-vector-of? check-pair-of? check-procedure-of?

                      integer boolean char cplxnum eof fixnum float
                      number list null number pair input-port output-port
                      procedure ratnum string symbol keyword vector blob pointer *)
       ((_ name (arg-type ...) ())
        (: name (arg-type ...) -> *))
       ((_ name (arg-type ...) (return-type ...))
        (: name (arg-type ... -> (return-type ...))))
       ((_ name (arg-type ...) (return-type ...) check-any? other-returns ...)
        (%declare-checked-fn/return name (arg-type ...) (return-type ... *) other-returns ...))
       ((_ name (arg-type ...) (return-type ...) fixnum? other-returns ...)
        (%declare-checked-fn/return name (arg-type ...) (return-type ... fixnum) other-returns ...))
       ((_ name (arg-type ...) (return-type ...) flonum? other-returns ...)
        (%declare-checked-fn/return name (arg-type ...) (return-type ... float) other-returns ...))
       ((_ name (arg-type ...) (return-type ...) integer? other-returns ...)
        (%declare-checked-fn/return name (arg-type ...) (return-type ... integer) other-returns ...))
       ((_ name (arg-type ...) (return-type ...) exact-integer? other-returns ...)
        (%declare-checked-fn/return name (arg-type ...) (return-type ... integer) other-returns ...))
       ((_ name (arg-type ...) (return-type ...) boolean? other-returns ...)
        (%declare-checked-fn/return name (arg-type ...) (return-type ... boolean) other-returns ...))
       ((_ name (arg-type ...) (return-type ...) char? other-returns ...)
        (%declare-checked-fn/return name (arg-type ...) (return-type ... char) other-returns ...))
       ((_ name (arg-type ...) (return-type ...) complex? other-returns ...)
        (%declare-checked-fn/return name (arg-type ...) (return-type ... cmlxnum) other-returns ...))
       ((_ name (arg-type ...) (return-type ...) eof? other-returns ...)
        (%declare-checked-fn/return name (arg-type ...) (return-type ... eof) other-returns ...))
       ((_ name (arg-type ...) (return-type ...) inexact? other-returns ...)
        (%declare-checked-fn/return name (arg-type ...) (return-type ... float) other-returns ...))
       ((_ name (arg-type ...) (return-type ...) real? other-returns ...)
        (%declare-checked-fn/return name (arg-type ...) (return-type ... number) other-returns ...))
       ((_ name (arg-type ...) (return-type ...) list? other-returns ...)
        (%declare-checked-fn/return name (arg-type ...) (return-type ... list) other-returns ...))
       ((_ name (arg-type ...) (return-type ...) (check-list-of? _) other-returns ...)
        (%declare-checked-fn/return name (arg-type ...) (return-type ... list) other-returns ...))
       ((_ name (arg-type ...) (return-type ...) null? other-returns ...)
        (%declare-checked-fn/return name (arg-type ...) (return-type ... null) other-returns ...))
       ((_ name (arg-type ...) (return-type ...) number? other-returns ...)
        (%declare-checked-fn/return name (arg-type ...) (return-type ... number) other-returns ...))
       ((_ name (arg-type ...) (return-type ...) pair? other-returns ...)
        (%declare-checked-fn/return name (arg-type ...) (return-type ... pair) other-returns ...))
       ((_ name (arg-type ...) (return-type ...) (check-pair-of? _ _) other-returns ...)
        (%declare-checked-fn/return name (arg-type ...) (return-type ... pair) other-returns ...))
       ((_ name (arg-type ...) (return-type ...) input-port? other-returns ...)
        (%declare-checked-fn/return name (arg-type ...) (return-type ... input-port) other-returns ...))
       ((_ name (arg-type ...) (return-type ...) output-port? other-returns ...)
        (%declare-checked-fn/return name (arg-type ...) (return-type ... output-port) other-returns ...))
       ((_ name (arg-type ...) (return-type ...) procedure? other-returns ...)
        (%declare-checked-fn/return name (arg-type ...) (return-type ... procedure) other-returns ...))
       ((_ name (arg-type ...) (return-type ...) (check-procedure-of? _ _) other-returns ...)
        (%declare-checked-fn/return name (arg-type ...) (return-type ... procedure) other-returns ...))
       ((_ name (arg-type ...) (return-type ...) rational? other-returns ...)
        (%declare-checked-fn/return name (arg-type ...) (return-type ... ratnum) other-returns ...))
       ((_ name (arg-type ...) (return-type ...) string? other-returns ...)
        (%declare-checked-fn/return name (arg-type ...) (return-type ... string) other-returns ...))
       ((_ name (arg-type ...) (return-type ...) symbol? other-returns ...)
        (%declare-checked-fn/return name (arg-type ...) (return-type ... symbol) other-returns ...))
       ((_ name (arg-type ...) (return-type ...) keyword? other-returns ...)
        (%declare-checked-fn/return name (arg-type ...) (return-type ... keyword) other-returns ...))
       ((_ name (arg-type ...) (return-type ...) vector? other-returns ...)
        (%declare-checked-fn/return name (arg-type ...) (return-type ... vector) other-returns ...))
       ((_ name (arg-type ...) (return-type ...) (check-vector-of? _) other-returns ...)
        (%declare-checked-fn/return name (arg-type ...) (return-type ... vector) other-returns ...))
       ((_ name (arg-type ...) (return-type ...) pointer? other-returns ...)
        (%declare-checked-fn/return name (arg-type ...) (return-type ... pointer) other-returns ...))
       ((_ name (arg-type ...) (return-type ...) pred other-returns ...)
        (%declare-checked-fn/return name (arg-type ...) (return-type ... *) other-returns ...))))
   (define-syntax %declare-checked-fn
     (syntax-rules (: ->
                      any? integer? boolean? char? complex?
                      fixnum? flonum?
                      eof? inexact? real?
                      list? null? number? pair?
                      input-port? output-port?
                      procedure? rational?
                      string? symbol? keyword? vector? pointer?
                      check-any? check-list-of? check-vector-of? check-pair-of? check-procedure-of?

                      integer boolean char cplxnum eof fixnum float
                      number list null number pair input-port output-port
                      procedure ratnum string symbol keyword vector blob pointer *)
       ((_ name (return ...) () (type ...))
        (%declare-checked-fn/return name (type ...) () return ...))
       ((_ name return ((arg fixnum?) check ...) (type ...))
        (%declare-checked-fn name return (check ...) (type ... fixnum)))
       ((_ name return ((arg flonum?) check ...) (type ...))
        (%declare-checked-fn name return (check ...) (type ... float)))
       ((_ name return ((arg integer?) check ...) (type ...))
        (%declare-checked-fn name return (check ...) (type ... number)))
       ((_ name return ((arg boolean?) check ...) (type ...))
        (%declare-checked-fn name return (check ...) (type ... boolean)))
       ((_ name return ((arg char?) check ...) (type ...))
        (%declare-checked-fn name return (check ...) (type ... char)))
       ((_ name return ((arg complex?) check ...) (type ...))
        (%declare-checked-fn name return (check ...) (type ... cplxnum)))
       ((_ name return ((arg eof?) check ...) (type ...))
        (%declare-checked-fn name return (check ...) (type ... eof)))
       ((_ name return ((arg inexact?) check ...) (type ...))
        (%declare-checked-fn name return (check ...) (type ... float)))
       ((_ name return ((arg real?) check ...) (type ...))
        (%declare-checked-fn name return (check ...) (type ... number)))
       ((_ name return ((arg list?) check ...) (type ...))
        (%declare-checked-fn name return (check ...) (type ... list)))
       ((_ name return ((arg null?) check ...) (type ...))
        (%declare-checked-fn name return (check ...) (type ... null)))
       ((_ name return ((arg number?) check ...) (type ...))
        (%declare-checked-fn name return (check ...) (type ... number)))
       ((_ name return ((arg pair?) check ...) (type ...))
        (%declare-checked-fn name return (check ...) (type ... pair)))
       ((_ name return ((arg input-port?) check ...) (type ...))
        (%declare-checked-fn name return (check ...) (type ... input-port)))
       ((_ name return ((arg output-port?) check ...) (type ...))
        (%declare-checked-fn name return (check ...) (type ... output-port)))
       ((_ name return ((arg procedure?) check ...) (type ...))
        (%declare-checked-fn name return (check ...) (type ... procedure)))
       ((_ name return ((arg rational?) check ...) (type ...))
        (%declare-checked-fn name return (check ...) (type ... ratnum)))
       ((_ name return ((arg string?) check ...) (type ...))
        (%declare-checked-fn name return (check ...) (type ... string)))
       ((_ name return ((arg symbol?) check ...) (type ...))
        (%declare-checked-fn name return (check ...) (type ... symbol)))
       ((_ name return ((arg keyword?) check ...) (type ...))
        (%declare-checked-fn name return (check ...) (type ... keyword)))
       ((_ name return ((arg vector?) check ...) (type ...))
        (%declare-checked-fn name return (check ...) (type ... vector)))
       ((_ name return ((arg pointer?) check ...) (type ...))
        (%declare-checked-fn name return (check ...) (type ... pointer)))
       ((_ name return (arg check ...) (type ...))
        (%declare-checked-fn name return (check ...) (type ... *)))))
   (define-syntax declare-checked
     (syntax-rules (=>)
       ((_ name predicate)
        (%declare-checked-var name predicate))
       ((_ (name args ...) => (return ...))
        (%declare-checked-fn name (return ...) (args ...)))
       ((_ (name args ...))
        (%declare-checked-fn name () (args ...))))))
  (else
   (define-syntax declare-checked
     (syntax-rules ()
       ((_ name predicate)
        (when #f #f))
       ((_ (name . args))
        (when #f #f))
       ((_ (name . args) => return)
        (when #f #f))))))

;; TODO: Should these even be used instead of
;; https://srfi.schemers.org/srfi-235/srfi-235.html?
(define (check-or? predicate1 . predicates)
  (lambda (object)
    (let rec ((predicates (cons predicate1 predicates)))
      (if (null? predicates)
          #f
          (or ((car predicates) object)
              (rec (cdr predicates)))))))

(define (check-and? predicate1 . predicates)
  (lambda (object)
    (let rec ((predicates (cons predicate1 predicates)))
      (if (null? predicates)
          #t
          (and ((car predicates) object)
               (rec (cdr predicates)))))))

(define (check-not? predicate)
  (lambda (object)
    (not (predicate object))))

(define (check-eqv? constant)
  (lambda (object)
    (eqv? object constant)))

(define (check-memv? . members)
  (lambda (object)
    (not (not (memv object members)))))

(define check-list-of?
  (case-lambda
    ((predicate)
     (check-list-of? predicate null?))
    ((predicate tail-predicate)
     (lambda (list)
       (check-arg list? list 'check-list-of?)
       (let rec ((list list))
         (if (not (pair? list))
             (tail-predicate list)
             (and (predicate (car list))
                  (rec (cdr list)))))))))

(define (check-vector-of? predicate)
  (lambda (vector)
    (check-arg vector? vector 'check-vector-of?)
    (let rec ((idx 0))
      (if (= idx (vector-length vector))
          #t
          (and (predicate (vector-ref vector idx))
               (rec (+ 1 idx)))))))

(define (check-pair-of? car-predicate cdr-predicate)
  (lambda (pair)
    (check-arg pair? pair 'check-pair-of?)
    (and (car-predicate (car pair))
         (cdr-predicate (cdr pair)))))

(define (check-procedure-of? arg-predicates return-predicates)
  (lambda (object)
    (procedure? object)))

(define (check-any? object)
  #t)
