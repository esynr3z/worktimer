(use-modules (tests helpers))

(test "start with task writes running record"
  (lambda ()
    (with-temp-home
      (lambda (home)
        (let* ((result (run-cli "start" "alpha/build"))
               (code (car result))
               (out (cadr result))
               (ts (read-file (timesheet-path home))))
          (assert-equal 0 code)
          (assert-contains out "--- NEW TASK RUN")
          (assert-contains out "alpha/build:")
          (assert-contains ts "--- TIMESHEET")
          (assert-regexp ts
                         "alpha/build: \\[[0-9][0-9][0-9][0-9]-[0-9][0-9]-[0-9][0-9] [0-9][0-9]:[0-9][0-9]:[0-9][0-9]\\]"))))))

(test "start without task uses last path"
  (lambda ()
    (with-fixture "timesheet-base.txt"
      (lambda (home)
        (let* ((result (run-cli "start"))
               (out (cadr result))
               (ts (read-file (timesheet-path home))))
          (assert-contains out "alpha/build:")
          (assert-regexp ts
                         "alpha/build: \\[[0-9][0-9][0-9][0-9]-[0-9][0-9]-[0-9][0-9] [0-9][0-9]:[0-9][0-9]:[0-9][0-9]\\]"))))))

(test "start without task on empty sheet prints error"
  (lambda ()
    (with-temp-home
      (lambda (home)
        (let* ((result (run-cli "start"))
               (out (cadr result)))
          (assert-contains out "Not specified task path")
          (assert-false (file-exists? (timesheet-path home))
                        "Expected no timesheet file"))))))

(test "stop without running task prints error and keeps file"
  (lambda ()
    (with-fixture "timesheet-base.txt"
      (lambda (home)
        (let* ((path (timesheet-path home))
               (before (read-file path))
               (result (run-cli "stop"))
               (out (cadr result))
               (after (read-file path)))
          (assert-contains out "Nothing to stop")
          (assert-equal before after))))))

(test "stop running task writes completed record"
  (lambda ()
    (with-temp-home
      (lambda (home)
        (run-cli "start" "alpha/build")
        (let* ((result (run-cli "stop"))
               (out (cadr result))
               (ts (read-file (timesheet-path home))))
          (assert-contains out "--- STOP TASK")
          (assert-contains out "alpha/build:")
          (assert-regexp ts
                         "alpha/build: \\[[0-9][0-9][0-9][0-9]-[0-9][0-9]-[0-9][0-9] [0-9][0-9]:[0-9][0-9]:[0-9][0-9]\\] - \\[[0-9][0-9][0-9][0-9]-[0-9][0-9]-[0-9][0-9] [0-9][0-9]:[0-9][0-9]:[0-9][0-9]\\] - [0-9][0-9]:[0-9][0-9]:[0-9][0-9]"))))))
