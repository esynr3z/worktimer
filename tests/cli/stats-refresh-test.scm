(use-modules (tests helpers))

(test "stats shows day-of-week averages"
  (lambda ()
    (with-fixture "timesheet-base.txt"
      (lambda (home)
        (let* ((result (run-cli "stats"))
               (out (cadr result)))
          (assert-contains out "By day-of-week statistics for all projects")
          (assert-contains out "FRI: 00:45:00")
          (assert-contains out "SAT: 01:30:00")
          (assert-contains out "SUN: 00:15:00"))))))

(test "stats filter by project path"
  (lambda ()
    (with-fixture "timesheet-base.txt"
      (lambda (home)
        (let* ((result (run-cli "stats" "alpha"))
               (out (cadr result)))
          (assert-contains out "statistics for alpha")
          (assert-contains out "FRI: 00:45:00")
          (assert-contains out "SUN: 00:15:00")
          (assert-not-contains out "SAT: 01:30:00"))))))

(test "refresh creates backup"
  (lambda ()
    (with-fixture "timesheet-base.txt"
      (lambda (home)
        (let* ((path (timesheet-path home))
               (bak (string-append path ".bak")))
          (run-cli "refresh")
          (assert-true (file-exists? bak) "Expected .timesheet.bak")
          (assert-contains (read-file bak) "--- TIMESHEET"))))))
