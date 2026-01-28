(use-modules (tests helpers))

(test "tasklist excludes archived"
  (lambda ()
    (with-fixture "timesheet-base.txt"
      (lambda (home)
        (let* ((result (run-cli "tasklist"))
               (out (cadr result)))
          (assert-contains out "alpha")
          (assert-contains out "alpha/build")
          (assert-contains out "alpha/test")
          (assert-not-contains out "beta"))))))

(test "archlist prints archive paths"
  (lambda ()
    (with-fixture "timesheet-base.txt"
      (lambda (home)
        (let* ((result (run-cli "archlist"))
               (out (cadr result)))
          (assert-contains out "beta"))))))

(test "dmenu shows stop when running"
  (lambda ()
    (with-temp-home
      (lambda (home)
        (run-cli "start" "alpha/build")
        (let* ((result (run-cli "dmenu"))
               (out (cadr result)))
          (assert-contains out "-- STOP alpha/build")
          (assert-contains out "alpha")
          (assert-contains out "alpha/build"))))))

(test "dmenu shows start when idle"
  (lambda ()
    (with-fixture "timesheet-base.txt"
      (lambda (home)
        (let* ((result (run-cli "dmenu"))
               (out (cadr result)))
          (assert-contains out "-- START alpha/build")
          (assert-contains out "alpha/build"))))))
