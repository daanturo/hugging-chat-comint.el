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

;; (defcustom hugging-chat-comint-model-list
;;   '("OpenAssistant/oasst-sft-6-llama-30b-xor")
;;   "List of Hugging Chat models.
;; See https://huggingface.co/chat."
;;   :group 'hugging-chat-comint)

;; (defcustom hugging-chat-comint-default-model
;;   "OpenAssistant/oasst-sft-6-llama-30b-xor"
;;   "Default model to use."
;;   :group 'hugging-chat-comint)

(defun hugging-chat-comint-default-shell-name-project-dedicated ()
  (format "%s %s"
          'hugging-chat-comint-shell
          (or (-some--> (project-current) (project-name it))
              (abbreviate-file-name default-directory))))

(defcustom hugging-chat-comint-shell-name
  (format "*%s*" "hugging-chat-comint-shell")
  "String (or function to be called with 0 arguments) to name the comint buffer.
The final name will wrapped with \"*\"s. To have per-project
buffers, set this to
`hugging-chat-comint-default-shell-name-project-dedicated' (but
currently doesn't seem to work as the session is still shared)."
  :group 'hugging-chat-comint)

(defun hugging-chat-comint-shell-setup-buffer ()
  (visual-line-mode))

(defun hugging-chat-comint-shell--buffer ()
  (-->
   (cond
    ((functionp hugging-chat-comint-shell-name)
     (funcall hugging-chat-comint-shell-name))
    ((stringp hugging-chat-comint-shell-name)
     hugging-chat-comint-shell-name))))

;; (defun hugging-chat-comint-read-model-name ()
;;   (completing-read
;;    "Hugging Chat model: "
;;    (lambda (string pred action)
;;      (if (eq action 'metadata)
;;          `(metadata (display-sort-function . identity))
;;        (complete-with-action
;;         action hugging-chat-comint-model-list string pred)))))

(defcustom hugging-chat-comint-cookies nil
  "JSON string or cookies file path."
  :group 'hugging-chat-comint
  :type 'string)

(defcustom hugging-chat-comint-prompt-indicator nil
  nil
  :group 'hugging-chat-comint
  :type 'string)

;;;###autoload
(defun hugging-chat-comint-make-shell-buffer ()
  (cl-assert (stringp hugging-chat-comint-cookies))
  (-let* ((comint-name (hugging-chat-comint-shell--buffer))
          (buf
           (apply #'make-comint-in-buffer
                  comint-name comint-name hugging-chat-comint-script-path nil
                  `(,(expand-file-name hugging-chat-comint-cookies)
                    ,@(and hugging-chat-comint-prompt-indicator
                           (list
                            "--prompt" hugging-chat-comint-prompt-indicator))))))
    (with-current-buffer buf
      (hugging-chat-comint-shell-setup-buffer))
    buf))

;;;###autoload
(defun hugging-chat-comint-switch-to-shell-buffer ()
  "https://github.com/Soulter/hugging-chat-api."
  (interactive)
  (-let* ((buf-name (hugging-chat-comint-shell--buffer)))
    (unless (get-buffer buf-name)
      (hugging-chat-comint-make-shell-buffer))
    (pop-to-buffer buf-name)))

;;; hugging-chat-comint.el ends here

(provide 'hugging-chat-comint)
