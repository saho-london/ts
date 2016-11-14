
;**** CHAPTER 12 {Scheme ���󥿥ץ꥿} ****

;**** Section 12.1 {���󥿥ץ꥿�γ���} ****

;** 12.1.1 {�������} **

;** 12.1.2 {���󥿥ץ꥿�ι���} **

;**** Section 12.2 {���󥿥ץ꥿�ץ����} ****

;** 12.2.1 {���󥿥ץ꥿�ν������REP �롼��} **

;;; ��³�� ts --- ���󥿥ץ꥿���ư����
(define (ts)
  (newline)
  (display "TS (TINY SCHEME IN SCHEME)")
  (newline)
  (ts:reset-environ)                  ; �Ķ�����������
  (ts:intern-primitive-procedures)    ; ���ܼ�³������Ͽ�򤹤�
  (ts:read-eval-print-loop)           ; REP�롼�פ�����
  #t)

; �ȥåץ�٥�ؤη�³ (���顼���ʤɤˡ���ɽ�Ū�ʼ�³��æ�Ф˻Ȥ���)
(define *ts:top-level-continuation* #f)
;;; REP�롼��
(define (ts:read-eval-print-loop)
  (letrec
    ((loop    ; REP�롼��
       (lambda ()
         ; ��³�� ts:read-eval-print ���³�դ��ǸƤӽФ�
         (if (call-with-current-continuation ts:read-eval-print)
           (loop)      ; ����ˡ�REP �򤹤롣
           (begin      ; #f ���֤����ȡ����󥿥ץ꥿��λ����
             (display "GOOD BYE.")
             (newline))))))
    (loop)))  ; REP�롼�פ�����

(define (ts:read-eval-print cont)
  (set! *ts:top-level-continuation* cont)
  (ts:prompt)  ; �ץ��ץȤ�ɽ�����롣
  (let ((user-input (ts:read-user-input)))  ; �����ɤ߹���
    (let ((val (ts:eval user-input (ts:top-environ))))
                                 ; �ȥåץ�٥�δĶ��Ǽ���ɾ������
      (ts:print-value val)       ; ɾ����̤�ɽ������
      #t)))
(define *ts:scheme-system-has-force-output* #f) ; �����Ϥ˹�碌��
(define (ts:prompt)
  (display "] ")
  (if *ts:scheme-system-has-force-output*
      (force-output))
  #t)

(define ts:error
  (lambda msg
    (letrec
      ((loop
         (lambda (s)
           (if (null? s)
             (begin (newline) (newline)
               (*ts:top-level-continuation* #t)) ;�ȥåץ�٥��
             (begin
               (display (car s)) (display " ")
               (loop (cdr s)))))))
      (newline)
      (display "TS ERROR")
      (newline)
      (loop msg))))

;** 12.2.2 {���󥿥ץ꥿�����Υǡ���ɽ��} **

(define *ts:null-obj*     (cons 'TS-TAG:NULL '()))
(define (ts:null? ts-obj) (eq? ts-obj *ts:null-obj*))

(define *ts:true*         (cons 'TS-TAG:BOOLEAN #t))
(define *ts:false*        (cons 'TS-TAG:BOOLEAN #f))
(define (ts:make-boolean scheme-obj)
  (if scheme-obj
    *ts:true*
    *ts:false*))

(define (ts:boolean? ts-obj) (eq? (car ts-obj) 'TS-TAG:BOOLEAN))

(define (ts:make-integer n)  (cons 'TS-TAG:INTEGER n))
(define (ts:integer? ts-obj) (eq? (car ts-obj) 'TS-TAG:INTEGER))

(define (ts:make-string s) (cons 'TS-TAG:STRING s))
(define (ts:string? ts-obj) (eq? (car ts-obj) 'TS-TAG:STRING))

(define (ts:make-symbol scheme-obj)
  (cons 'TS-TAG:SYMBOL scheme-obj))
(define (ts:symbol? ts-obj) 
  (eq? (car ts-obj) 'TS-TAG:SYMBOL))

(define (ts:get-scheme-value ts-obj)  (cdr ts-obj))

(define (ts:cons s1 s2)   (cons 'TS-TAG:PAIR (cons s1 s2)))
(define (ts:pair? ts-obj) (eq? (car ts-obj) 'TS-TAG:PAIR))
(define (ts:car s)    (cadr s))
(define (ts:cdr s)    (cddr s))

(define (ts:cadr s)   (ts:car (ts:cdr s)))
(define (ts:cddr s)   (ts:cdr (ts:cdr s)))
(define (ts:caddr s)  (ts:car (ts:cddr s)))
(define (ts:cadddr s) (ts:cadr (ts:cddr s)))

(define (ts:make-compound-procedure ts-parameter-list body env)
  (letrec
    ((param-rev-loop
       (lambda (ts-params rev-params nargs)
         (if (ts:null? ts-params)
           (list 'TS-TAG:COMP-PROC rev-params nargs body env)
           (param-rev-loop
             (ts:cdr ts-params)
             (cons (ts:car ts-params) rev-params)
             (+ nargs 1))))))
    (param-rev-loop ts-parameter-list '() 0)))

(define (ts:compound-procedure? ts-obj)
  (eq? (car ts-obj) 'TS-TAG:COMP-PROC))

(define (ts:get-arglist-compound-procedure proc) 
  (list-ref proc 1))
(define (ts:get-nargs-compound-procedure proc)
  (list-ref proc 2))
(define (ts:get-body-compound-procedure proc)
  (list-ref proc 3))
(define (ts:get-env-compound-procedure proc)
  (list-ref proc 4))

(define (ts:make-primitive-procedure proc name nargs)
  (list 'TS-TAG:PRIM-PROC proc name nargs))

(define (ts:primitive-procedure? ts-obj)
  (eq? (car ts-obj) 'TS-TAG:PRIM-PROC))

(define (ts:get-body-primitive-procedure proc)   (cadr proc))
(define (ts:get-name-primitive-procedure proc)   (caddr proc))
(define (ts:get-nargs-primitive-procedure proc)  (cadddr proc))

(define (ts:scheme-obj->ts-obj scheme-obj)
  (cond
    ((null? scheme-obj)    *ts:null-obj*)
    ((boolean? scheme-obj) (ts:make-boolean scheme-obj))
    ((integer? scheme-obj) (ts:make-integer scheme-obj))
    ((string? scheme-obj) (ts:make-string scheme-obj))
    ((symbol? scheme-obj)  (ts:make-symbol  scheme-obj))
    ((pair? scheme-obj)
      (ts:cons (ts:scheme-obj->ts-obj (car scheme-obj))
               (ts:scheme-obj->ts-obj (cdr scheme-obj))))
    (else (ts:error "ILLEGAL INPUT:" scheme-obj))))

;** 12.2.3 {�����ɤ߹��ߤ�ɽ��} **

(define (ts:read-user-input)
  (let ((scheme-obj (read)))   ; �ޤ� Scheme �ǡ����Ȥ����ɤ߹���
    (if (eof-object? scheme-obj) 
      ; ���Ϥ�����ʤ�ȥåץ�٥�� #f ���֤���(ts ����λ����)
      (*ts:top-level-continuation* #f)
      ; ���Ϥ�����С��ǡ����Ѵ�������Τ��֤�
      (ts:scheme-obj->ts-obj scheme-obj)))) 

(define (ts:print-value ts-exp)  
  (ts:print-exp ts-exp)
  (newline))
(define (ts:print-exp ts-exp)
  (cond
    ((or (ts:boolean? ts-exp) (ts:integer? ts-exp) (ts:string? ts-exp)
       (ts:symbol? ts-exp) (ts:null? ts-exp))
     (display (ts:get-scheme-value ts-exp)))
    ((ts:primitive-procedure? ts-exp)
     (display "<primitive-procedure:")
     (display (ts:get-name-primitive-procedure ts-exp))
     (display ">"))
    ((ts:compound-procedure? ts-exp)
     (display "<compound-procedure>"))
    ((ts:pair? ts-exp)
     (display "(")
     (ts:print-exp (ts:car ts-exp)) 
     (display " . ")     ; �ɥåȵ�ˡ�ˤ��ɽ��
     (ts:print-exp (ts:cdr ts-exp))
     (display ")"))))

;** 12.2.4 {ɾ���� (����1)} **


(define (ts:eval exp env)
  (cond
    ((ts:null? exp)         ;** ���ꥹ�� 
     exp)                   ; ����ɾ��Ū�ǡ����ʤΤǡ����켫�Ȥ��֤�
    ((ts:boolean? exp)      ;** �֡���ǡ���
     exp)                   ; ����ɾ��Ū�ǡ����ʤΤǡ����켫�Ȥ��֤�
    ((ts:integer? exp)      ;** ����
     exp)                   ; ����ɾ��Ū�ǡ����ʤΤǡ����켫�Ȥ��֤�
    ((ts:string? exp)       ;** ʸ����
     exp)                   ; ����ɾ��Ū�ǡ����ʤΤǡ����켫�Ȥ��֤�
    ((ts:symbol? exp)       ;** ����
     (ts:lookup-binding     ; «����Ĵ�٤ơ��ѿ��ͤ򸫤Ĥ���
       exp env))
    ((ts:pair? exp)         ;** ��  (�ü�����ޤ��ϼ�³���ƤӽФ�)
     (if (ts:special-form? exp)
       (ts:do-special-form exp env)   ; �ü����
       (ts:do-application exp env)))  ; ��³���ƤӽФ�
    (else                   ; �ФƤ���Ϥ��Τʤ��ǡ���
      (ts:error "ILLEGAL OBJECT:" exp))))

;** 12.2.5 {�ѿ��Ȥ�����  (����)} **

;** 12.2.6 {ɾ���� (����2 �ü����)} **

(define (ts:special-form? exp)
  (case (ts:get-scheme-value (ts:car exp)) 
    ((QUOTE IF BEGIN DEFINE SET! LAMBDA) #t)
    (else                                #f)))

(define (ts:do-special-form exp env)
  (case (ts:get-scheme-value (ts:car exp))
    ((QUOTE)                    ;** (QUOTE OBJ)�ξ��
     (ts:cadr exp))
    ((IF)                       ;** (IF CON EXP1 EXP2)�ξ��
     (let ((con (ts:eval (ts:cadr exp) env)))
       (if (not (eq? con *ts:false*)) 
         (ts:eval (ts:caddr exp) env)
         (ts:eval (ts:cadddr exp) env))))
    ((BEGIN)                    ;**(BEGIN EXP1 ...)�ξ��
     (ts:eval-begin (ts:cdr exp) env)) 
    ((DEFINE)                   ;**(DEFINE VAR EXP)�ξ��
     (ts:define-var
       (ts:cadr exp) (ts:eval (ts:caddr exp) env) env)
     (ts:cadr exp))
    ((SET!)                     ;**(SET! VAR EXP)�ξ��
     (let ((val (ts:eval (ts:caddr exp) env)))
       (ts:set-var! (ts:cadr exp) val env)
       val))
    ((LAMBDA)                   ;**(LAMBDA (VAR) EXP1...)�ξ��
     (ts:make-compound-procedure 
       (ts:cadr exp) (ts:cddr exp) env))
    (else                       ; �ФƤ���Ϥ��Τʤ��ǡ���
      (ts:error "ILLEGAL SPECIAL FORM:" exp))))

(define (ts:eval-begin exps env)
  (letrec
    ((begin-loop  
       (lambda (rest last)
         (if (ts:null? (ts:cdr rest))
             (ts:eval (ts:car rest) env)
             (begin-loop
              (ts:cdr rest)
              (ts:eval (ts:car rest) env))))))
    (if (ts:null? exps)
        *ts:null-obj*
        (begin-loop exps *ts:null-obj*))))

;** 12.2.7 {��³���ƤӽФ�} **

(define (ts:do-application exp env)
  (let ((proc (ts:eval (ts:car exp) env)) ;��³���ǡ�������Ф�
        (args (ts:eval-args (ts:cdr exp) env))) ;������ɾ������
    (cond
      ((ts:compound-procedure? proc)      ;** ʣ���³���ξ��
       (ts:do-application-compound exp env proc args))
      ((ts:primitive-procedure? proc)     ;** ���ܼ�³���ξ��
       (ts:do-application-primitive exp env proc args))
      (else
       (ts:error "WRONG TYPE TO APPLY")))))

(define (ts:eval-args args env)  
  (letrec
    ((ev-arg-loop
       (lambda (args results)
         (if (ts:null? args)
           results            ; ȿž�����ޤޤη�̤Υꥹ�Ȥ��֤�
           (ev-arg-loop
             (ts:cdr args)
             (cons (ts:eval (ts:car args) env) results))))))
    (ev-arg-loop args '())))

;** 12.2.8 {���ܼ�³���θƤӽФ�} **

(define (ts:do-application-primitive exp env proc args)
  (let ((proc-nargs (ts:get-nargs-primitive-procedure proc)))
    (if (or
          (eq? 'any proc-nargs)     ; �����ο���������������å�
          (= (length args) proc-nargs))
      (apply                        ; �����ο�����äƤ���С�
                                    ;   ���ܼ�³����ƤӽФ���
        (ts:get-body-primitive-procedure proc)
        (reverse args))
      (ts:error
        "ILLEGAL NUMBER OF ARGS TO"
        (ts:get-name-primitive-procedure proc)))))

;** 12.2.9 {ʣ���³���θƤӽФ�: �ɽ��ѿ���ʸ��Ūͭ����§} **

(define (ts:do-application-compound exp env proc args)
  (if (= (length args)                ; �����ο���������������å�
        (ts:get-nargs-compound-procedure proc))
    (let ((new-env                    ; �����ο�����äƤ���С�
            (ts:extend-environ        ;   �Ķ�����礷��
              (ts:get-arglist-compound-procedure proc)
              args (ts:get-env-compound-procedure proc))))
      (ts:eval-begin                  ;   ��³�������Τ�ɾ������
        (ts:get-body-compound-procedure proc) new-env))
    (ts:error
      "ILLEGAL NUMBER OF ARGS TO"
      (ts:get-name-primitive-procedure proc))))

(define (ts:extend-environ vars vals env)
  (letrec
    ((loop
       (lambda (vars vals bindings)
         (if (null? vars)      ; ���٤Ƥ��ѿ����Ф���«�������
           (cons bindings env) ;   ���Ȥ� ENV �ä��ơ��֤���
           (loop (cdr vars)    ; «�������Ȥ˲ä��Ƥ椯��
                 (cdr vals) 
                 (cons (cons (car vars) (car vals))  
                       bindings))))))
    (loop vars vals '())))

;** 12.2.10 {�ѿ��Ȥ����� (�¸�)} **

(define *ts:bindings*       #f)
(define (ts:reset-environ)  (set! *ts:bindings* (list '())))
(define (ts:top-environ)    *ts:bindings*)

(define (ts:lookup-binding var env)
  (let ((binding (ts:find-binding var env)))
    (if binding
      (cdr binding)
      (ts:error "UNBOUND VARIABLE:" 
        (ts:get-scheme-value var)))))

(define (ts:define-var var val env)
  (let* ((binding (ts:find-binding2 var env)))
    (if binding
      (set-cdr! binding val)
      (set-car! env (cons (cons var val) (car env)))))
  val)

(define (ts:set-var! var val env)
  (let* ((binding (ts:find-binding var env)))
    (if binding
      (set-cdr! binding val)
      (ts:error "UNBOUND VARIABLE:"
        (ts:get-scheme-value var)))))

(define (ts:find-binding var env)
  (letrec
    ((loop
       (lambda (env)
         (if (null? env)
           #f                   ; ���Ĥ���ʤ��ä��� #f ���֤�
           (let ((binding (assoc var (car env))))
             (if binding
               binding               ; ���Ĥ���С�����«�����֤�
               (loop (cdr env)))))))); �������Ȥ����õ��
    (loop env)))

(define (ts:find-binding2 var env)
  (assoc var (car env)))

;** 12.2.11 {���ܼ�³���μ¸�} **

(define (ts:intern-prim-proc name proc nargs)
  (ts:define-var
    (ts:scheme-obj->ts-obj name)
    (ts:make-primitive-procedure proc name nargs)
    (ts:top-environ)))  

(define (ts:intern-primitive-procedures)
  (ts:intern-prim-proc  'NULL?   tsp:null?  1)
  (ts:intern-prim-proc  'EQ?     tsp:eq?    2)
  (ts:intern-prim-proc  'CONS    tsp:cons   2)
  (ts:intern-prim-proc  'CAR     tsp:car    1)
  (ts:intern-prim-proc  'CDR     tsp:cdr    1)
  (ts:intern-prim-proc  'PAIR?   tsp:pair?  1)
  (ts:intern-prim-proc  '+       tsp:+      'any)
  (ts:intern-prim-proc  '-       tsp:-      'any)
  (ts:intern-prim-proc  '*       tsp:*      2)
  (ts:intern-prim-proc  '/       tsp:/      2)
  (ts:intern-prim-proc  '=       tsp:=      2)
  (ts:intern-prim-proc  '>       tsp:>      2)
  (ts:intern-prim-proc  'STRING-LENGTH      tsp:string-length      1)
  (ts:intern-prim-proc  'STRING=?           tsp:string=?           2)
  (ts:intern-prim-proc  'STRING-APPEND      tsp:string-append      2)
  (ts:intern-prim-proc  'QUIT    tsp:quit   0))

(define (tsp:cons s1 s2)
  (ts:cons s1 s2))
(define (tsp:car s)
  (ts:car s))
(define (tsp:cdr s)
  (ts:cdr s))
(define (tsp:null? s)
  (ts:make-boolean (ts:null? s)))
(define (tsp:eq? s1 s2)
  (ts:make-boolean 
    (eq? (ts:get-scheme-value s1) (ts:get-scheme-value s2))))
(define (tsp:pair? s)
  (ts:make-boolean (ts:pair? s)))
(define tsp:+
  (lambda args   ; ���������٤ƥꥹ�Ȥˤʤäơ�args ��Ϳ�����롣
    (letrec
      ((loop
         (lambda (nums sum)  
           ; nums �ϰ����ο��λĤꡢsum ���¤���ַ��
           (if (null? nums)
             (ts:make-integer sum)
             (loop (cdr nums)
               (+ (ts:get-scheme-value (car nums)) sum))))))
      (loop args 0))))
(define tsp:-
  (lambda args   ; ���������٤ƥꥹ�Ȥˤʤäơ�args ��Ϳ�����롣
    (if (null? args)
      (ts:error
        "ILLEGAL NUMBER OF ARGS TO tsp:-")
      (if (null? (cdr args))
        (ts:make-integer 
          (- (ts:get-scheme-value (car args))))
        (letrec
          ((loop
             (lambda (nums sum)  
               ; nums �ϰ����ο��λĤꡢsum ���¤���ַ��
               (if (null? nums)
                 (ts:make-integer sum)
                 (loop (cdr nums)
                   (+ (ts:get-scheme-value (car nums)) sum))))))
          (ts:make-integer 
            (- (ts:get-scheme-value (car args))
              (ts:get-scheme-value (loop (cdr args) 0)))))))))

(define (tsp:* n1 n2)
  (ts:make-integer 
   (* (ts:get-scheme-value n1) (ts:get-scheme-value n2))))
(define (tsp:/ n1 n2)
  (ts:make-integer 
   (quotient (ts:get-scheme-value n1) (ts:get-scheme-value n2))))
(define (tsp:= n1 n2)
  (ts:make-boolean 
    (= (ts:get-scheme-value n1) (ts:get-scheme-value n2))))
(define (tsp:> n1 n2)
  (ts:make-boolean 
    (> (ts:get-scheme-value n1) (ts:get-scheme-value n2))))
    
(define (tsp:string-length s)
  (ts:make-integer 
    (string-length (ts:get-scheme-value s))))
(define (tsp:string=? s1 s2)
  (ts:make-boolean
    (string=? (ts:get-scheme-value s1) (ts:get-scheme-value s2))))
(define (tsp:string-append s1 s2)
  (ts:make-string 
    (string-append (ts:get-scheme-value s1) (ts:get-scheme-value s2))))

(define (tsp:quit)
  (*ts:top-level-continuation* #f))

;**** Section 12.3 {���󥿥ץ꥿�μ¹�} ****

;**** Section 12.4 {����¾������} ****

;** 12.4.1 {�絭���δ����ȥ��١������쥯�����} **

;** 12.4.2 {��³} **
;; --- End of the Program ---
