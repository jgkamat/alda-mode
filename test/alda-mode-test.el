;;; alda-mode-test --- Tests for alda-mode

;;; Commentary:
;;; These are unit tests for alda mode, hopefully they increase the quality of
;;; future releases!

;;; Run like so: cask exec ert-runner

;; Thanks to: http://rejeep.github.io/emacs/cask/ert/ert-runner/ert-async/ecukes/testing/travis/2014/01/09/various-testing-tools-in-emacs.html

;;; Code:

(require 'el-mock)

(defmacro mock-alda-cmd (&rest args)
  "Mocks alda-run-cmd to input the following args.
ARGS the arguments to expect.
Note: Mocking this command assumes the alda-run-cmd function is working properly."
  `(mock (alda-run-cmd ,@args)))

(ert-deftest sanity-check-ert ()
  "Check if ERT is working. :)"
  (should t))

;; TODO enable this test once we start actually using alda in our tests
;; (ert-deftest alda-found ()
;;   "Check if alda is found."
;;   (should (alda-location))
;;   (should (string-match "alda$" (alda-location))))

(ert-deftest alda-play-text-test ()
  "Tests if alda-play-text is working correctly."
  (with-mock
    (mock-alda-cmd "play" "--history" "" "--code" "piano: c d e")
    (alda-play-text "piano: c d e")

    (mock-alda-cmd "play" "--history" "" "--code" "piano: c d
e f g")
    (alda-play-text "piano: c d
e f g")

    (mock-alda-cmd "play" "--history" "" "--code" "guitar: \"d e f\"")
    (alda-play-text "guitar: \"d e f\"")))

(ert-deftest alda-play-file-test ()
  "Tests if alda-play-text is working correctly."
  (with-mock
    (stub buffer-file-name => "hello-world.alda")
    (mock-alda-cmd "play" "--file" "hello-world.alda")
    (alda-play-file)))

(ert-deftest alda-play-buffer-test ()
  "Tests if alda-play-text is working correctly."
  (with-mock
    (stub buffer-string => "midi-square-wave: c d e")
    (mock-alda-cmd "play" "--history" "" "--code" "midi-square-wave: c d e")
    (alda-play-buffer)))

(provide 'alda-mode-test)

;;; alda-mode-test.el ends here
