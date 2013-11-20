; For convenience - can just (reload-cd-db) at the REPL to get the changes.
(defun reload-cd-db ()
  (load "~/Development/Git Repos/LISP/cd-db.lisp")
)

(defvar *db* nil)

(defun reset-db ()
  (setf *db* nil)
)

(defun save-db (filename)
  (with-open-file (out filename :direction :output :if-exists :supersede)
    (with-standard-io-syntax (print *db* out))
  )
)

(defun load-db (filename)
  (with-open-file (in filename)
    (with-standard-io-syntax (setf *db* (read in)))
  )
)

(defun make-cd (title artist year ripped)
  (list :title title :artist artist :year year :ripped ripped)
)

(defun add-record (cd)
  (push cd *db*)
)

(defun dump-db ()
  (dolist (cd *db*)
    (format t "~{~a:~10t~a~%~}~%" cd))
)

(defun prompt-read (prompt)
  (format *query-io* "~a: " prompt)
  (force-output *query-io*)
  (read-line *query-io*)
)

(defun prompt-for-cd ()
  (make-cd
    (prompt-read "Title")
    (prompt-read "Artist")
    (or (parse-integer (prompt-read "Year") :junk-allowed t) 0)
    (y-or-n-p "Ripped")
  )
)

(defun add-cds ()
  (loop (add-record (prompt-for-cd))
     (if (not (y-or-n-p "Another?"))
	 (return)
     )
   )
)

; Old non-macro way, but simpler to understand.
(defun match-cds-all (&key title artist year (ripped nil ripped-p))
  #'(lambda (cd)
    (and
      (if title (equal (getf cd :title) title) t)
      (if artist (equal (getf cd :artist) artist) t)
      (if year (equal (getf cd :year) year) t)
      (if ripped-p (equal (getf cd :ripped) ripped) t)
    )
  )
)

(defun select-cds (selector-fn)
  (remove-if-not selector-fn *db*)
)

; Old non-macro way, but simpler to understand.
(defun update-cds-all (selector-fn &key title artist year (ripped nil ripped-p))
  (setf *db*
    (mapcar
      #'(lambda (row)
	(when (funcall selector-fn row)
	  (if title (setf (getf row :title) title))
	  (if artist (setf (getf row :artist) artist))
	  (if year (setf (getf row :year) year))
	  (if ripped-p (setf (getf row :ripped) ripped))
	)
      row)
    *db*)
  )
)

(defun delete-cds (selector-fn)
  (setf *db*
    (remove-if selector-fn *db*)
  )
)

(defun make-cd-comparison-expr (row field value)
  `(equal (getf ,row ,field) ,value)
)

(defun make-cd-comparisons-list (row fields)
  (loop while fields
    collecting (make-cd-comparison-expr row (pop fields) (pop fields))
  )
)

; Produce a compound expression which will take a list and tests whether all
; the specified keys have the appropriate values. We read the input list as
; pairs of keys and values.
(defmacro match-cds (&rest clauses)
  `#'(lambda (cd)
    (and ,@(make-cd-comparisons-list 'cd clauses))
  )
)

(defun make-cd-update-expr (row field value)
  `(setf (getf ,row ,field) ,value)
)

(defun make-cd-updates-list (row fields)
  (loop while fields
    collecting (make-cd-update-expr row (pop fields) (pop fields))
  )
)

; Produce a compound expression which will take a list and set each key's value
; to the one given in our input list of key-value pairs.
(defmacro update-cd (&rest clauses)
  `#'(lambda (cd)
    ,@(make-cd-updates-list 'cd clauses)
  )
)

; Iterate over each list in the DB. For each one the selector matches, we
; use the input key-value pairs to set each key to its given values.
(defun update-cds(selector-fn &rest clauses)
  (setf *db*
    (mapcar
      #'(lambda (row)
	(when (funcall selector-fn row)
	  (funcall (update-cd clauses) row)
	)
      row)
    *db*)
  )
)
