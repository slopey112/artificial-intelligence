;; This code is copied from /misc/pattern-matching.lisp. Details on that can be found in that
;; directory, including a description of the below functions and their functionality. The actual
;; code for Student does not begin until line 194.

(setf (get '?is 'single-match) 'match-is)
(setf (get '?or 'single-match) 'match-or)
(setf (get '?and 'single-match) 'match-and)
(setf (get '?not 'single-match) 'match-not)

(setf (get '?* 'segment-match) 'segment-match)
(setf (get '?+ 'segment-match) 'segment-match+)
(setf (get '?? 'segment-match) 'segment-match?)
(setf (get '?if 'segment-match) 'match-if)

(defconstant fail nil
  "Indicates pat-mat does not match.")

(defconstant no-bindings '((t . t))
  "Indicates pat-mat success, but with no variables.")

(defun get-binding (var bindings)
  "Finds (variable . value) pair in a binding list."
  (assoc var bindings))

(defun binding-val (binding)
  "Finds value for binding pair."
  (cdr binding))

(defun make-binding (var val) (cons var val))

(defun lookup (var bindings)
  "Finds value of binding in bindings list."
  (binding-val (get-bindings var bindings)))

(defun extend-bindings (var val bindings)
  "Adds (var . val) pair to bindings list."
  (cons (cons var val) bindings))

(defun variable-p (x)
  "Is x a symbol that begins with '?' ?"
  (and (symbolp x) (eql (char (symbol-name x) 0) #\?)))

(defun segment-match-p (pattern)
  "Is pattern calling a segment match?"
  (and (consp pattern) (consp (first pattern))
    (symbolp (first (first pattern)))
    (segment-match-fn (first (first pattern)))))

(defun single-pattern-p (pattern)
  "Is pattern calling a single match?"
  (and (consp pattern)
    (single-pattern-fn (first pattern))))

(defun segment-matcher (pattern input bindings)
  "Calls correct segment matcher."
  (funcall (segment-match-fn (first (first pattern)))
    pattern input bindings))

(defun single-matcher (pattern input bindings)
  "Call correct single matcher."
  (funcall (single-pattern-fn (first pattern))
    (rest pattern) input bindings))

(defun segment-match-fn (x)
  "Gets segment-match function given x."
  (when (symbolp x) (get x 'segment-match)))

(defun single-pattern-fn (x)
  "Gets single-pattern function given x."
  (when (symbolp x) (get x 'single-match)))

(defun pat-match (pat in &optional (bindings no-bindings))
  "Checks if string matches pattern."
  (cond ((eq bindings fail) fail)
    ((variable-p pat)
      (match-variable pat in bindings))
    ((eql pat in) bindings)
    ((segment-match-p pat)
      (segment-matcher pat in bindings))
    ((single-pattern-p pat)
      (single-matcher pat in bindings))
    ((and (consp pat) (consp in))
      (pat-match (rest pat) (rest in)
        (pat-match (first pat) (first in) bindings)))
    (t fail)))

(defun match-variable (var input bindings)
  "If var matches input, returns bindings."
  (let ((binding (get-binding var bindings)))
    (cond ((not binding) (extend-bindings var input bindings))
      ((equal input (binding-val binding)) bindings)
        (t fail))))

(defun match-is (var-and-pred in bindings)
  "Success if in matches pred wherein pred requests funcall."
  (let* ((var (first var-and-pred))
    (pred (second var-and-red))
    (new-bindings (pat-match var in bindings)))
    (if (or (eq new-bindings fail)
      (not (funcall pred in)))
        fail
        new-bindings)))

(defun match-and (patterns in bindings)
  "Success if all patterns evaluate to true."
  (cond ((eq bindings fail) fail)
    ((null patterns) bindings)
    (t (match-and (rest patterns) in
      (pat-match (first patterns) in bindings)))))

(defun match-or (patterns in bindings)
  "Success if any pattern evaulates to true."
  (if (null patterns)
    fail
    (let ((new-bindings (pat-match (first patterns) in bindings)))
      (if (null new-bindings)
        (match-or (rest patterns) in bindings)
        new-bindings))))

(defun match-not (patterns in bindings)
  "Success if none of the patterns evaulates to true."
  (if (match-or patterns in bindings)
    fail
    bindings))

(defun segment-match (pattern input bindings &optional (start 0))
  "Matches input against a segment of input rather than a singular match."
  (let ((var (second (first pattern)))
    (pat (rest pattern)))
    (if (null pat)
      (match-variable var input bindings)
      (let ((pos (first-match-pos (first pat) input start)))
        (if (null pos)
          fail
          (let ((b2 (pat-match pat
            (subseq input pos)
            (match-variable var (subseq input 0 pos)
            bindings))))
            (if (eq b2 fail)
              (segment-match pattern input bindings (+ pos 1))
              b2)))))))

(defun first-match-pos (pat1 input start)
  "Find the first position that patl could possibly match input, starting at position start.
  If patl is non-constant, then just return start."
  (cond ((and (atom pat1) (not (variable-p pat1)))
      (position pat1 input :start start :test #'equal))
    ((< start (length input)) start)
    (t nil)))

(defun segment-match+ (pattern input bindings)
  "Matches one or more elements of input."
  (segment-match pattern input bindings 1))

(defun segment-match? (pattern input bindings)
  "Matches one or zero elements of input."
  (let ((var (second (first pattern)))
    (pat (rest pattern)))
    (or (pat-match (cons var pat) input bindings)
      (pat-match pattern input bindings))))

(defun match-if (pattern input bindings)
  "Evaluates pattern if expression true."
  (and (progv
    (mapcar #'car bindings)
    (mapcar #'cdr bindings)
    (eval (second (first pattern))))
    (pat-match (rest pattern) input bindings)))

(defun pat-match-abbrev (symbol expansion)
  "Define symbol as a macro standing for a pat-match pattern."
  (setf (get symbol 'expand-pat-match-abbrev)
    (expand-pat-match-abbrev expansion)))

(defun expand-pat-match-abbrev (pat)
  "Expand out all pattern matching abbreviations in pat."
  (cond ((and (symbolp pat)
      (get pat 'expand-pat-match-abbrev)))
    ((atom pat) pat)
    (t (cons (expand-pat-match-abbrev (first pat))
      (expand-pat-match-abbrev (rest pat))))))

(defun rule-based-translator (input rules &key (matcher #'pat-match)
  (rule-if #'first) (rule-then #'rest) (action #'sublis))
  "Find the first rule in rules that matches input, and apply the action to that rule."
  (some #'(lambda (rule)
  (let ((result (funcall matcher (funcall rule-if rule) input)))
    (if (not (eq result fail))
      (funcall action result (funcall rule-then rule)))))
    rules))

;; The actual code for student begins here.

(defstruct (rule (:type list)) pattern response)
(defstruct (exp (:type list) (:constructor mkexp (lhs op rhs))) lhs op rhs)
(defun exp-p (x) (consp x))
(defun exp-args (x) (rest x))
(defconstant operators-and-inverses '((+ -) (- +) (* /) (/ *) (= =)))

(pat-match-abbrev '?x* '(?* ?x))
(pat-match-abbrev '?y* '(?* ?y))

(defparameter *student-rules* (mapcar #'expand-pat-match-abbrev
  '(((?x* \.)                     ?x)
    ((?x* \. ?y*)            (?x ?y))
    ((if ?x* \, then ?y*)    (?x ?y))
    ((if ?x* then ?y*)       (?x ?y))
    ((if ?x* \, ?y*)         (?x ?y))
    ((find ?x* and ?y*)      ((= to-find-1 ?x) (= to-find-2 ?y))
    ((find ?x*)              (= to-find ?x))
    ((?x* equals ?y*)        (= ?x ?y))
    ((?x* same as ?y*)       (= ?x ?y))
    ((?x* = ?y*)             (= ?x ?y))
    ((?x* is equal to ?y*)   (= ?x ?y))
    ((?x* is ?y*)            (= ?x ?y))
    ((?x* - ?y*)             (- ?x ?y))
    ((?x* minus ?y*)         (- ?x ?y))
    ((difference between ?x* and ?y*) (- ?x ?y))
    ((difference ?x* ?y*)    (- ?x ?y))
    ((?x* + ?y*)             (+ ?x ?y))
    ((?x* plus ?y*)          (+ ?x ?y))
    ((sum ?x* and ?y*)       (+ ?x ?y))
    ((product ?x* and ?y*)   (* ?x ?y))
    ((?x* * ?y*)             (* ?x ?y))
    ((?x* times ?y*)         (* ?x ?y))
    ((?x* / ?y*)             (/ ?x ?y))
    ((?x* divided by ?y*)    (/ ?x ?y))
    ((?x* per ?y*)           (/ ?x ?y))
    ((half ?x*)               (/ ?x 2))
    ((one half ?x*)           (/ ?x 2))
    ((twice ?x*)              (* ?x 2))
    ((double ?x*)             (* ?x 2))
    ((square ?x*)             (* ?x ?x))))))

(defun student (words)
  "Solves algebraic word problems."
  (solve-equations (translate-to-expression (remove-if #'noise-word-p words))))

(defun solve-equations (equations)
  "Print the equations and their solution"
  (print "hi")
  (print-equations "The equations to be solved are:" equations)
  (print-equations "The solution is:" (solve equations nil)))

(defun translate-to-expression (words)
  "Translate an English phrase into an equation or expression."
  (or (rule-based-translator
    words *student-rules*
    :rule-if #'rule-pattern :rule-then #'rule-response
    :action #'(lambda (bindings response)
      (sublis (mapcar #'translate-pair bindings) response)))
  (make-variable words)))

(defun translate-pair (pair)
  "Translates initial binding into expression."
  (cons (car pair)
    (if (eq (cdr pair) t)
      (translate-to-expression (list 't))
      (translate-to-expression (cdr pair)))))

(defun create-list-of-equations (exp)
  "Seperates out equations embedded in nested pairs."
  (cond ((null exp) nil)
    ((atom (first exp)) (list exp))
    (t (append (create-list-of-equations (first exp))
      (create-list-of-equations (rest exp))))))

(defun make-variable (words)
  "Creates a variable name based on the words."
  ;; This assumes that noise words have been previously removed.
  (first words))

(defun noise-word-p (word)
  "Checks if word can be safely ignored."
  (member word '(a an the this number of $)))

(defun solve (equations known)
  "Solves equation through constraint propogation."
  (or (some #'(lambda (equation)
    (let ((x (one-unknown equation)))
      (when x
        (let ((answer (solve-arithmetic (isolate equation x))))
          (solve (subst (exp-rhs answer) (exp-lhs answer)
            (remove equation equations))
            (cons answer known))))))
      equations)
    known))

(defun isolate (e x)
  "Isolates an equation."
  ;; Case I: X = A -> X = n
  (cond ((eq (exp-lhs e) x)
      e)
    ;; Case II: A = f(x) -> f(x) = A
    ((in-exp x (exp-rhs e))
      (isolate (mkexp (exp-rhs e) '= (exp-lhs e)) x))
    ;; Case III: f(x) * A = B -> f(x) = B / A
    ((in-exp (exp-lhs (exp-lhs e)))
      (isolate (mkexp (exp-lhs (exp-lhs e)) '=
        (mkexp (exp-rhs e)
          (inverse-op (exp-op (exp-lhs e)))
          (exp-rhs (exp-lhs e)))) x))
    ;; Case IV: A * f(x) = B -> f(x) = B / A
    ((commutative-p (exp-op (exp-lhs e)))
      (isolate (mkexp (exp-rhs (exp-lhs e)) '=
        (mkexp (exp-rhs e)
          (inverse-op (exp-op (exp-lhs e)))
          (exp-lhs (exp-lhs e)))) x))
    ;; Case V: A / f(x) = B -> f(x) = A / B
    (t
      (isolate (mkexp (exp-rhs (exp-lhs e)) '=
        (mkexp (exp-lhs (exp-lhs e))
          (exp-op (exp-lhs e))
          (exp-rhs e))) x))))

(defun inverse-op (op)
  "Finds corresponding inverse operator. (EX + returns -)"
  (second (assoc op operators-and-inverses)))

(defun unknown-p (exp)
  (symbolp exp))

(defun in-exp (x exp)
  "True if x is in the expression."
  (or (eq x exp)
    (and (exp-p exp)
      (or (in-exp x (exp-lhs exp)) (in-exp x (exp-rhs exp))))))

(defun no-unknown (exp)
  "True if exp has no unknowns."
  (cond ((unknown-p exp) nil)
    ((atom exp) t)
    ((no-unknown (exp-rhs exp)) (no-unknown (exp-lhs exp)))
    (t nil)))

(defun one-unknown (exp)
  "If there is exactly one unknown, returns such."
  (cond ((unknown-p exp) exp)
    ((atom exp) nil)
    ((no-unknown (exp-lhs exp)) (one-unknown (exp-rhs exp)))
    ((no-unknown (exp-rhs exp)) (one-unknown (exp-lhs exp)))
    (t nil)))

(defun commutative-p (op)
  "True if operator is commutative."
  (member op '(+ * =)))

(defun solve-arithmetic (exp)
  "Solves arithmetic on right hand side."
  (mkexp (exp-lhs exp) '= (eval (exp-rhs exp))))

(defun binary-exp-p (x)
  (and (exp-p x) (= (length (exp-args x)) 2)))

(defun prefix->infix (exp)
  "Translate prefix to infix expressions."
  (if (atom exp) exp
    (mapcar #'prefix->infix
      (if (binary-exp-p exp)
        (list (exp-lhs exp) (exp-op exp) exp)))))

(defun print-equations (header equations)
  "Print a list of equations."
  (format t "~%~a~{~% ~{ ~a~}~}~%" header
    (mapcar #'prefix->infix equations)))
;(trace translate-pair translate-to-expression)
(solve-equations '((= (+ 3 4) (* (- 5 (+ 2 x)) 7)) (= (+ (* 3 X) y) 12)))
(student '(If the number of customers Tom gets is twice the square of the number of advertisements he runs \,
  and the number of advertisements is 45 \,
  then what is the number of customers Tom gets ?))
