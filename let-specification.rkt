
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

<\subsubsection*{3.12 - Cond}>
<\subsubsection*{Grammar}>

Expression ::= cond {Expression ==> Expression}* end

<\subsubsection*{Specification}>

(value-of (cond-exp exps) env)
= (if (null? exps)
      (error)
      (if (expval->bool (value-of (caar exps) env))
          (value-of (caddar exps) env)
          (value-of (cond-exp (cdr exps)) env)))
