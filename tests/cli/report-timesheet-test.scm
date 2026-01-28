(use-modules (tests helpers))

(test "report shows totals and excludes archived"
  (lambda ()
    (with-fixture "timesheet-base.txt"
      (lambda (home)
        (let* ((result (run-cli "report"))
               (out (cadr result)))
          (assert-contains out "--- REPORT")
          (assert-contains out "Overall: 01:45:00")
          (assert-contains out "alpha: 01:45:00")
          (assert-contains out "build: 00:45:00")
          (assert-contains out "test: 01:00:00")
          (assert-not-contains out "beta")
          (assert-contains out "LAST STOPPED"))))))

(test "report day filter"
  (lambda ()
    (with-fixture "timesheet-base.txt"
      (lambda (home)
        (let* ((result (run-cli "report" "day" "2024-01-05"))
               (out (cadr result)))
          (assert-contains out "DAY 2024-01-05")
          (assert-contains out "Overall: 01:30:00")
          (assert-contains out "build: 00:30:00")
          (assert-contains out "test: 01:00:00"))))))

(test "report week filter"
  (lambda ()
    (with-fixture "timesheet-base.txt"
      (lambda (home)
        (let* ((result (run-cli "report" "week" "2024-01-05"))
               (out (cadr result)))
          (assert-contains out "WEEK [2024-01-01 - 2024-01-08)")
          (assert-contains out "Overall: 01:45:00"))))))

(test "report month filter"
  (lambda ()
    (with-fixture "timesheet-base.txt"
      (lambda (home)
        (let* ((result (run-cli "report" "month" "2024-01-05"))
               (out (cadr result)))
          (assert-contains out "MONTH 2024-01")
          (assert-contains out "Overall: 01:45:00"))))))

(test "report project filter"
  (lambda ()
    (with-fixture "timesheet-base.txt"
      (lambda (home)
        (let* ((result (run-cli "report" "alpha/build"))
               (out (cadr result)))
          (assert-contains out "PROJECT alpha/build")
          (assert-contains out "Overall: 00:45:00")
          (assert-not-contains out "alpha/test"))))))

(test "timesheet shows raw entries without archived"
  (lambda ()
    (with-fixture "timesheet-base.txt"
      (lambda (home)
        (let* ((result (run-cli "timesheet"))
               (out (cadr result)))
          (assert-contains out "--- TIMESHEET")
          (assert-contains out "alpha/build:")
          (assert-contains out "alpha/test:")
          (assert-not-contains out "beta/review"))))))

(test "timesheet day filter"
  (lambda ()
    (with-fixture "timesheet-base.txt"
      (lambda (home)
        (let* ((result (run-cli "timesheet" "day" "2024-01-05"))
               (out (cadr result)))
          (assert-contains out "DAY 2024-01-05")
          (assert-contains out "2024-01-05 09:00:00")
          (assert-contains out "2024-01-05 10:00:00")
          (assert-not-contains out "2024-01-07 09:00:00"))))))

(test "timesheet project filter"
  (lambda ()
    (with-fixture "timesheet-base.txt"
      (lambda (home)
        (let* ((result (run-cli "timesheet" "alpha"))
               (out (cadr result)))
          (assert-contains out "PROJECT alpha")
          (assert-contains out "alpha/build:")
          (assert-contains out "alpha/test:")
          (assert-not-contains out "beta/review"))))))
