;; Test helpers for worktimer CLI
(define-module (tests helpers)
  #:use-module (ice-9 format)
  #:use-module (ice-9 popen)
  #:use-module (ice-9 ftw)
  #:use-module (ice-9 regex)
  #:export (test
            print-test-summary
            assert-true
            assert-false
            assert-equal
            assert-contains
            assert-not-contains
            assert-regexp
            with-temp-home
            with-fixture
            run-cli
            read-file
            write-file
            timesheet-path
            fixture-path
            section-lines
            repo-root
            tests-root
            fixtures-root))

(define repo-root (getcwd))
(define tests-root (string-append repo-root "/tests"))
(define fixtures-root (string-append tests-root "/fixtures"))

(define test-results '())

(define (string-prefix? prefix str)
  (let ((plen (string-length prefix))
        (slen (string-length str)))
    (and (<= plen slen)
         (string=? prefix (substring str 0 plen)))))

(define (string-suffix? suffix str)
  (let ((slen (string-length str))
        (tlen (string-length suffix)))
    (and (<= tlen slen)
         (string=? suffix (substring str (- slen tlen) slen)))))

(define (string-contains? str sub)
  (let ((slen (string-length str))
        (tlen (string-length sub)))
    (let loop ((i 0))
      (cond
       ((> (+ i tlen) slen) #f)
       ((string=? sub (substring str i (+ i tlen))) #t)
       (else (loop (+ i 1)))))))

(define (string-join items sep)
  (let loop ((items items) (out ""))
    (cond
     ((null? items) out)
     ((string=? out "") (loop (cdr items) (car items)))
     (else (loop (cdr items) (string-append out sep (car items)))))))

(define (record-result name ok? message)
  (set! test-results (cons (list name ok? message) test-results)))

(define (error-message key args)
  (cond
   ((null? args) (format #f "~a" key))
   ((and (= (length args) 1) (string? (car args))) (car args))
   (else (format #f "~a ~s" key args))))

(define (test name thunk)
  (catch #t
    (lambda ()
      (thunk)
      (record-result name #t ""))
    (lambda (key . args)
      (record-result name #f (error-message key args))))
  #t)

(define (count pred lst)
  (let loop ((lst lst) (n 0))
    (if (null? lst)
        n
        (loop (cdr lst) (+ n (if (pred (car lst)) 1 0))))))

(define (print-test-summary)
  (let* ((results (reverse test-results))
         (total (length results))
         (failed (count (lambda (r) (not (cadr r))) results)))
    (display "--- TESTS\n")
    (for-each
     (lambda (r)
       (let ((name (car r))
             (ok? (cadr r))
             (message (caddr r)))
         (format #t "~a ~a" (if ok? "ok" "not ok") name)
         (when (and (not ok?) (not (string=? message "")))
           (format #t " - ~a" message))
         (newline)))
     results)
    (format #t "\nTotal: ~a, Failed: ~a\n" total failed)
    (exit (if (> failed 0) 1 0))))

(define (assert-true value message)
  (unless value
    (throw 'test-failed message)))

(define (assert-false value message)
  (when value
    (throw 'test-failed message)))

(define (assert-equal expected actual)
  (unless (equal? expected actual)
    (throw 'test-failed
           (format #f "Expected ~s, got ~s" expected actual))))

(define (assert-contains str sub)
  (unless (string-contains? str sub)
    (throw 'test-failed
           (format #f "Expected output to include '~a'" sub))))

(define (assert-not-contains str sub)
  (when (string-contains? str sub)
    (throw 'test-failed
           (format #f "Expected output to exclude '~a'" sub))))

(define (assert-regexp str pattern)
  (let ((rx (make-regexp pattern)))
    (unless (regexp-exec rx str)
      (throw 'test-failed
             (format #f "Expected output to match /~a/" pattern)))))

(define (port->string port)
  (let loop ((chars '()))
    (let ((c (read-char port)))
      (if (eof-object? c)
          (list->string (reverse chars))
          (loop (cons c chars))))))

(define (status-exit-code status)
  (if (integer? status)
      (quotient status 256)
      0))

(define (shell-quote str)
  (let loop ((i 0) (out "'"))
    (if (= i (string-length str))
        (string-append out "'")
        (let ((ch (string-ref str i)))
          (if (char=? ch #\')
              (loop (+ i 1) (string-append out "'\"'\"'"))
              (loop (+ i 1) (string-append out (string ch))))))))

(define (run-cli . args)
  (let* ((script (string-append repo-root "/worktimer.scm"))
         (cmd (string-append
               "guile "
               (shell-quote script)
               (if (null? args)
                   ""
                   (string-append " " (string-join (map shell-quote args) " ")))
               " 2>&1"))
         (port (open-pipe* OPEN_READ "sh" "-c" cmd))
         (output (port->string port))
         (status (close-pipe port)))
    (list (status-exit-code status) output)))

(define (read-file path)
  (if (file-exists? path)
      (call-with-input-file path port->string)
      ""))

(define (write-file path content)
  (call-with-output-file path
    (lambda (port)
      (display content port))))

(define (timesheet-path home)
  (string-append home "/.timesheet"))

(define (fixture-path name)
  (string-append fixtures-root "/" name))

(define temp-counter 0)

(define (make-temp-dir)
  (set! temp-counter (+ temp-counter 1))
  (let* ((base (or (getenv "TMPDIR") "/tmp"))
         (path (string-append base "/worktimer-test-"
                              (number->string (getpid))
                              "-" (number->string temp-counter))))
    (mkdir path)
    path))

(define (remove-tree path)
  (when (file-exists? path)
    (if (file-is-directory? path)
        (let ((entries (scandir path (lambda (n) (not (member n '("." "..")))))))
          (for-each (lambda (entry)
                      (remove-tree (string-append path "/" entry)))
                    entries)
          (rmdir path))
        (delete-file path))))

(define (with-temp-home thunk)
  (let ((prev (getenv "WORKTIMER_HOME"))
        (tmp (make-temp-dir)))
    (dynamic-wind
      (lambda ()
        (setenv "WORKTIMER_HOME" tmp)
        (setenv "GUILE_AUTO_COMPILE" "0"))
      (lambda ()
        (thunk tmp))
      (lambda ()
        (if prev
            (setenv "WORKTIMER_HOME" prev)
            (unsetenv "WORKTIMER_HOME"))
        (remove-tree tmp)))))

(define (with-fixture fixture-name thunk)
  (with-temp-home
    (lambda (home)
      (write-file (timesheet-path home) (read-file (fixture-path fixture-name)))
      (thunk home))))

(define (split-lines str)
  (let loop ((i 0) (start 0) (out '()))
    (if (= i (string-length str))
        (reverse (if (< start i) (cons (substring str start i) out) out))
        (if (char=? (string-ref str i) #\newline)
            (loop (+ i 1) (+ i 1) (cons (substring str start i) out))
            (loop (+ i 1) start out)))))

(define (section-lines content header)
  (let loop ((lines (split-lines content)) (in-section #f) (out '()))
    (if (null? lines)
        (reverse out)
        (let ((line (car lines)))
          (cond
           ((string-prefix? "--- " line)
            (if (string=? line header)
                (loop (cdr lines) #t out)
                (loop (cdr lines) #f out)))
           (in-section (loop (cdr lines) #t (cons line out)))
           (else (loop (cdr lines) #f out)))))))
