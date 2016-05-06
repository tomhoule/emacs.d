;;; utils --- utility functions for my init.el
;;; Commentary:

;;; Code:

(defun moo () (print "moo"))

(defun tom/call-terminal ()
  "Start a terminal, depending on the OS."
  (call-process "gnome-terminal" nil nil))

(provide 'utils)
;;; utils.el ends here
