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
