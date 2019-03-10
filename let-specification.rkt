
(value-of (const-exp n) env) = $\lceil$n$\rceil$

(value-of (var-exp var) env) = (apply-env env var)

(value-of (diff-exp exp1 exp2) env)
= $\lceil$ $\lfloor$value-of exp1 env$\rfloor$ - $\lfloor$value-of exp2 env$\rfloor$ $\rceil$

(value-of (if-exp exp1 exp2 exp3) env)
= (if $\lfloor$value-of exp1 env$\rfloor$
      (value-of exp2 env)
      (value-of exp3 env))

<\subsection*{3.6 - Negation}>
<\subsubsection*{Grammar}>

Expression ::= minus (Expression)

<\subsubsection*{Specification}>

(value-of (minus-exp exp) env)
= (num-val (* -1
              (expval->num (value-of exp env))))

<\subsection*{3.7 - Numeric Predicates}>
<\subsubsection*{Grammar}>

Expression ::= equal? (Expression, Expression)
           ::= greater? (Expression, Expression)
           ::= less? (Expression, Expression)

<\subsubsection*{Specification}>

(value-of ({operator}-exp exp1 exp2) env)
= (bool-val ({operator} (expval->bool (value-of exp1 env))
                        (expval->bool (value-of exp2 env))))

<\subsection*{3.8 - Arithmetic}>
<\subsubsection*{Grammar}>

Expression ::= + (Expression, Expression)
           ::= * (Expression, Expression)
           ::= / (Expression, Expression)

<\subsubsection*{Specification}>

(value-of ({operator}-exp exp1 exp2) env)
= (num-val ({operator} (expval->num (value-of exp1 env))
                       (expval->num (value-of exp2 env))))

<\subsection*{3.9 - List processing}>
<\subsubsection*{Grammar}>

Expression ::= cons (Expression, Expression)
           ::= car (Expression)
           ::= cdr (Expression)
           ::= null? (Expression)
           ::= emptylist

<\subsubsection*{Interface for the list datatype}>

expval->list : ExpVal $\rightarrow$ List
list-val     : ExpVal $\times$ ExpVal $\rightarrow$ ExpVal
head-val     : ExpVal $\rightarrow$ ExpVal
tail-val     : ExpVal $\rightarrow$ ExpVal
empty-val    : ExpVal

<\subsubsection*{Interpreter specification}>

(value-of (cons-exp exp1 exps) env)
= (list-val (value-of exp1 env)
            (value-of exp2 env))

(value-of (car-exp exp) env)
= (head-val (value-of exp env))

(value-of (cdr-exp exp) env)
= (tail-val (value-of exp env))

(value-of (emptylist-exp) env)
= (empty-val)

(value-of (null?-exp exp) env)
= (let* ((val (value-of exp env))
         (ls (expval->list val)))
    (bool-val (null? ls)))


<\subsection*{3.10 - List construction}>
<\subsubsection*{Grammar}>

Expression ::= list ({Expression}*)

<\subsubsection*{Specification}>

(value-of (list-exp exps) env)
= (if (null? exps)
      (empty-val)
      (list-val (value-of (car exps) env)
                (value-of (list-exp (cdr exps)) env)))

<\subsection*{3.12 - Cond}>
<\subsubsection*{Grammar}>

Expression ::= cond {Expression ==> Expression}* end

<\subsubsection*{Specification}>

(value-of (cond-exp preds conseqs) env)
= (if (null? preds)
      (error)
      (if (expval->bool (value-of (car preds) env))
          (value-of (car conseqs) env)
          (value-of (cond-exp (cdr preds) (cdr conseqs)) env)))

<\subsection*{3.14 - Boolean expressions (Int is now the only expressed value}>
<\subsubsection*{Grammar}>

Bool-expression ::= equal? (Expression, Expression)
                ::= less? (Expression, Expression)
                ::= greater? (Expression, Expression)
                ::= zero? (Expression)
                ::= null? (Expression)

Expression ::= if Bool-expression then Expression else Expression
           ::= cond {Bool-expression ==> Expression}* end

<\subsubsection*{Specification}>

(value-of-bool-exp (equal?-exp exp1 exp2) env)
= (= (expval->num (value-of exp1 env))
     (expval->num (value-of exp2 env)))

(value-of-bool-exp (zero?-exp exp) env)
= (zero? (expval->num (value-of exp env)))

(value-of-bool-exp (null?-exp exp) env)
= (null? (expval->list (value-of exp env)))

<\subsection*{3.16 - Let}>
<\subsubsection*{Grammar}>

Expression ::= let {Identifier = Expression}* in Expression

<\subsubsection*{Specification}>

(value-of (let-exp vars exps final) env)
= (let ((vals (map (lambda (e) (value-of e env)) exps)))
    (value-of final (append-env vars vals env)))
