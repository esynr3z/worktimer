(use-modules (tests helpers))

(test "current shows running task"
  (lambda ()
    (with-temp-home
      (lambda (home)
        (run-cli "start" "alpha/build")
        (let* ((result (run-cli "current"))
               (out (cadr result)))
          (assert-regexp out
                         "alpha/build: [0-9][0-9]:[0-9][0-9]:[0-9][0-9]"))))))

(test "current shows NO TASKS when idle"
  (lambda ()
    (with-temp-home
      (lambda (home)
        (let* ((result (run-cli "current"))
               (out (cadr result)))
          (assert-contains out "NO TASKS"))))))

(test "no command prints current and last task"
  (lambda ()
    (with-fixture "timesheet-base.txt"
      (lambda (home)
        (let* ((result (run-cli))
               (out (cadr result)))
          (assert-contains out "NO TASKS")
          (assert-contains out "Last task: alpha/build"))))))
