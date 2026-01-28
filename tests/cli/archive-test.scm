(use-modules (tests helpers))

(test "archive list shows archived tasks"
  (lambda ()
    (with-fixture "timesheet-base.txt"
      (lambda (home)
        (let* ((result (run-cli "archive"))
               (out (cadr result)))
          (assert-contains out "--- ARCHIVE")
          (assert-contains out "beta"))))))

(test "archive add and unarch remove"
  (lambda ()
    (with-fixture "timesheet-base.txt"
      (lambda (home)
        (run-cli "archive" "alpha/test")
        (let* ((ts (read-file (timesheet-path home)))
               (archive-lines (section-lines ts "--- ARCHIVE")))
          (assert-true (member "alpha/test" archive-lines)
                       "Expected alpha/test in archive section"))
        (run-cli "unarch" "alpha/test")
        (let* ((ts (read-file (timesheet-path home)))
               (archive-lines (section-lines ts "--- ARCHIVE")))
          (assert-false (member "alpha/test" archive-lines)
                        "Expected alpha/test removed from archive section"))))))
