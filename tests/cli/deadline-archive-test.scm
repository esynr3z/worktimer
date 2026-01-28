(use-modules (tests helpers))

(test "deadline all shows all deadlines"
  (lambda ()
    (with-fixture "timesheet-base.txt"
      (lambda (home)
        (let* ((result (run-cli "deadline" "all"))
               (out (cadr result)))
          (assert-contains out "alpha: 2024-01-31")
          (assert-contains out "alpha/build: 02:00:00")
          (assert-contains out "beta/review: 01:00:00"))))))

(test "deadline without args uses last task"
  (lambda ()
    (with-fixture "timesheet-base.txt"
      (lambda (home)
        (let* ((result (run-cli "deadline"))
               (out (cadr result)))
          (assert-contains out "alpha/build: 02:00:00")
          (assert-not-contains out "alpha: 2024-01-31"))))))

(test "deadline set updates last task"
  (lambda ()
    (with-fixture "timesheet-base.txt"
      (lambda (home)
        (run-cli "deadline" "set" "00:30:00")
        (let ((ts (read-file (timesheet-path home))))
          (assert-contains ts "alpha/build: 00:30:00")
          (assert-not-contains ts "alpha/build: 02:00:00"))))))

(test "deadline set adds new task deadline"
  (lambda ()
    (with-fixture "timesheet-base.txt"
      (lambda (home)
        (run-cli "deadline" "set" "gamma/plan" "2024-02-10")
        (let ((ts (read-file (timesheet-path home))))
          (assert-contains ts "gamma/plan: 2024-02-10"))))))

(test "deadline clear removes deadline"
  (lambda ()
    (with-fixture "timesheet-base.txt"
      (lambda (home)
        (run-cli "deadline" "set" "00:30:00")
        (let* ((result (run-cli "deadline" "clear" "alpha/build"))
               (out (cadr result))
               (ts (read-file (timesheet-path home))))
          (assert-contains out "Deleted alpha/build")
          (assert-not-contains ts "alpha/build: 00:30:00"))))))

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
