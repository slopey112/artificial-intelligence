;; Genereic pattern matching tool that allows for boolean operators, segment matching, and single matching.

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
  (funcall (segment-match-fn (first pattern))
    (rest pattern) input bindings))

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
      (segment-match pat in bindings))
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
      (let ((pos (first-match-pos (first pat) input bindings)))
        (if (null pos)
          fail
          (let ((b2 (pat-match pat
            (subseq input pos)
            (match-variable var (subseq input 0 pos))
            bindings)))
            (if (eq b2 fail)
              (segment-match pattern input bindings (+ pos 1))
              bindings)))))))

(defun first-match-pos (patl input start)
  "Find the first position that patl could possibly match input, starting at position start.
  If patl is non-constant, then just return start."
  (cond ((and (atom patl) (not (variable-p patl)))
    (position patl input :start start :test #'equal))
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
