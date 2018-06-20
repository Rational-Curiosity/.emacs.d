(when (bug-check-function-bytecode
       'byte-compile-cond-jump-table
       "wwEhiUBAAUBBAkFAxImJiYmJiQYHhfcABgdHxVU/hfcAxscGC8jJyssGDp6DNgAGDUdTgjkABg1HJgayB8wgsgLMILIDzQYJIYjOBgchiM/QIYgIGNHSAyKIKcsGCJ6DcQDLBgieQUCyAdMGCMUisggGB4mD0gCJQMwgsgaJQLIIiUFAsgfUBgYhiNUGCAYHBgsjiAiJARgGCISlANbJCSKIgqsA1wYJCSKICAFVhLwACAFUVYS8ANjZIYjR0gYIIogGBkHEoYgptgIBQbaCgnMAiNQCIYiJg+kAidcBCSKIxIkRtgKC7QDaxCGI1AMhiAYGCkKJEoc=")
  (defconst byte-compile--default-val (cons nil nil) "A unique object.")

  (defun byte-compile-cond-jump-table-info (clauses)
    "If CLAUSES is a `cond' form where:
The condition for each clause is of the form (TEST VAR VALUE).
VAR is a variable.
TEST and VAR are the same throughout all conditions.
VALUE satisfies `macroexp-const-p'.
Return a list of the form ((TEST . VAR)  ((VALUE BODY) ...))"
    (let ((cases '())
          (ok t)
          prev-var prev-test)
      (and (catch 'break
             (dolist (clause (cdr clauses) ok)
               (let* ((condition (car clause))
                      (test (car-safe condition))
                      (vars (when (consp condition)
                              (byte-compile-cond-vars (cadr condition) (cl-caddr condition))))
                      (obj1 (car-safe vars))
                      (obj2 (cdr-safe vars))
                      (body (cdr-safe clause)))
                 (unless prev-var
                   (setq prev-var obj1))
                 (unless prev-test
                   (setq prev-test test))
                 (if (and obj1 (memq test '(eq eql equal))
                          (consp condition)
                          (eq test prev-test)
                          (eq obj1 prev-var)
                          ;; discard duplicate clauses
                          (not (assq obj2 cases)))
                     (push (list (if (consp obj2) (eval obj2) obj2) body) cases)
                   (if (and (macroexp-const-p condition) condition)
                       (progn (push (list byte-compile--default-val
                                          (or body `(,condition)))
                                    cases)
                              (throw 'break t))
                     (setq ok nil)
                     (throw 'break nil))))))
           (list (cons prev-test prev-var) (nreverse cases)))))

  (defun byte-compile-cond-jump-table (clauses)
    (let* ((table-info (byte-compile-cond-jump-table-info clauses))
           (test (caar table-info))
           (var (cdar table-info))
           (cases (cadr table-info))
           jump-table test-obj body tag donetag default-tag default-case)
      (when (and cases (not (= (length cases) 1)))
        ;; TODO: Once :linear-search is implemented for `make-hash-table'
        ;; set it to `t' for cond forms with a small number of cases.
        (setq jump-table (make-hash-table
                          :test test
                          :purecopy t
                          :size (if (assq byte-compile--default-val cases)
                                    (1- (length cases))
                                  (length cases)))
              default-tag (byte-compile-make-tag)
              donetag (byte-compile-make-tag))
        ;; The structure of byte-switch code:
        ;;
        ;; varref var
        ;; constant #s(hash-table purecopy t data (val1 (TAG1) val2 (TAG2)))
        ;; switch
        ;; goto DEFAULT-TAG
        ;; TAG1
        ;; <clause body>
        ;; goto DONETAG
        ;; TAG2
        ;; <clause body>
        ;; goto DONETAG
        ;; DEFAULT-TAG
        ;; <body for `t' clause, if any (else `constant nil')>
        ;; DONETAG

        (byte-compile-variable-ref var)
        (byte-compile-push-constant jump-table)
        (byte-compile-out 'byte-switch)

        ;; When the opcode argument is `byte-goto', `byte-compile-goto' sets
        ;; `byte-compile-depth' to `nil'. However, we need `byte-compile-depth'
        ;; to be non-nil for generating tags for all cases. Since
        ;; `byte-compile-depth' will increase by at most 1 after compiling
        ;; all of the clause (which is further enforced by cl-assert below)
        ;; it should be safe to preserve its value.
        (let ((byte-compile-depth byte-compile-depth))
          (byte-compile-goto 'byte-goto default-tag))

        (let ((default-match (assq byte-compile--default-val cases)))
          (when default-match
            (setq default-case (cadr default-match)
                  cases (butlast cases))))

        (dolist (case cases)
          (setq tag (byte-compile-make-tag)
                test-obj (nth 0 case)
                body (nth 1 case))
          (byte-compile-out-tag tag)
          (puthash test-obj tag jump-table)

          (let ((byte-compile-depth byte-compile-depth)
                (init-depth byte-compile-depth))
            ;; Since `byte-compile-body' might increase `byte-compile-depth'
            ;; by 1, not preserving its value will cause it to potentially
            ;; increase by one for every clause body compiled, causing
            ;; depth/tag conflicts or violating asserts down the road.
            ;; To make sure `byte-compile-body' itself doesn't violate this,
            ;; we use `cl-assert'.
            (if (null body)
                (byte-compile-form t byte-compile--for-effect)
              (byte-compile-body body byte-compile--for-effect))
            (cl-assert (or (= byte-compile-depth init-depth)
                           (= byte-compile-depth (1+ init-depth))))
            (byte-compile-goto 'byte-goto donetag)
            (setcdr (cdr donetag) nil)))

        (byte-compile-out-tag default-tag)
        (if default-case
            (byte-compile-body-do-effect default-case)
          (byte-compile-constant nil))
        (byte-compile-out-tag donetag)
        (push jump-table byte-compile-jump-tables)))))


(provide 'bytecomp-bug)
