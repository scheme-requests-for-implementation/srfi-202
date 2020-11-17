#lang racket
(require racket/match)

(provide #%module-begin #%app #%datum syntax-rules
         (rename-out [and-let*/match and-let*]))

(define-syntax match-lambda-rest
  (syntax-rules ()
    ((match-lambda-rest (first second . rest)
			(processed ...) body ...)
     (match-lambda-rest (second . rest)
			(processed ... first) body ...))

    ((match-lambda-rest (last) (processed ...)
			body ...)
     (match-lambda* ((list processed ... last) body ...)
                    (_ #f)))

    ((match-lambda-rest (last . tail)
			(processed ...) body ...)
     (match-lambda* ((list-rest processed ... last tail)
		     body ...)
                    (_ #f)))

    ((match-lambda-rest last-tail () body ...)
     (match-lambda* (last-tail body ...) (_ #f)))
    ))


(define-syntax and-let*/match
  (lambda (stx)
    (syntax-case stx (values)
      
      ((_)
       #'#t)
      
      ((_ ())
       #'#t)
      
      ((_ () body ...)
       #'(let () body ...))
      
      ((_ ((name binding) rest ...) body ...)
       (identifier? #'name)
       #'(let ((name binding))
           (and name
                (and-let*/match (rest ...)
                                body ...))))
      ((_ (((values . structure) binding) rest ...)
	  body ...)
       #'(call-with-values (lambda () binding)
           (match-lambda-rest
	    structure ()
            (and-let*/match (rest ...)
                            body ...))))
      
      ((_ ((value binding) rest ...) body ...)
       #'(match binding
           (value
            (and-let*/match (rest ...)
                            body ...))
           (_ #f)))
      
      ((_ ((condition) rest ...)
          body ...)
       #'(and condition
              (and-let*/match (rest ...)
                              body ...)))
      
      ((_ ((value * ... expression) rest ...)
	  body ...)
       (identifier? #'value)
       #'(call-with-values
	     (lambda () expression)
           (match-lambda-rest
	    (value * ... . _) ()
            (and value
                 (and-let*/match (rest ...)
                                 body ...)))))
      
      ((_ ((value ... expression) rest ...) body ...)
       #'(call-with-values
	     (lambda () expression)
           (match-lambda-rest
	    (value ... . _) ()
            (and-let*/match (rest ...)
                            body ...))))      
      )))
