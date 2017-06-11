;;; alda-mode-tests --- Tests for alda-mode

;;; Commentary:
;;; These are unit tests for alda mode, hopefully they increase the quality of
;;; future releases!

;;; Run like so: `emacs -batch -l ert -l alda-mode.el -l alda-mode-tests.el -f ert-run-tests-batch-and-exit`

;;; Code:


(ert-deftest sanity-check-ert ()
  "Check if ERT is working. :)"
  (should t))

(ert-deftest alda-found ()
  "Check if alda is found."
  (should (alda-location))
  (should (string-match "alda$" (alda-location))))


(provide 'alda-mode-tests)

;;; alda-mode-tests.el ends here
