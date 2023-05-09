;; -*- lexical-binding: t; -*-

;;; Commentary:
;;


;;; Code:

(require 'compat)

(require 'dash)
(require 's)

(defvar hugging-chat-comint--load-path
  (file-name-parent-directory (or load-file-name buffer-file-name)))

(defvar hugging-chat-comint-script-path
  (file-name-concat hugging-chat-comint--load-path "hugging-chat-comint.py"))

(defvar hugging-chat-comint-shell-name-function
  (lambda ()
    (format "%s %s"
            'hugging-chat-comint-shell
            (or (-some--> (project-current) (project-name it))
                (abbreviate-file-name default-directory)))))

;;;###autoload
(defun hugging-chat-comint-shell ()
  (interactive)
  (-let* ((name (funcall hugging-chat-comint-shell-name-function))
          (buf (make-comint name hugging-chat-comint-script-path nil)))
    (with-current-buffer buf
      (visual-line-mode))
    (pop-to-buffer buf)))

;;; hugging-chat-comint.el ends here

(provide 'hugging-chat-comint)
