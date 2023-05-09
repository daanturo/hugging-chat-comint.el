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

(defcustom hugging-chat-comint-model-list
  '("OpenAssistant/oasst-sft-6-llama-30b-xor"
    "BigCode/StarCoderBase" ; currently doesn't work :v
    )
  "List of Hugging Chat models.
See https://huggingface.co/chat."
  :group 'hugging-chat-comint)

(defcustom hugging-chat-comint-default-model
  "OpenAssistant/oasst-sft-6-llama-30b-xor"
  "Default model to use, set this to skip prompting.
See `hugging-chat-comint-model-list'."
  :group 'hugging-chat-comint)

(defun hugging-chat-comint-default-shell-name-project-dedicated ()
  (format "%s %s"
          'hugging-chat-comint-shell
          (or (-some--> (project-current) (project-name it))
              (abbreviate-file-name default-directory))))

(defcustom hugging-chat-comint-shell-name "hugging-chat-comint-shell"
  "String (or function to be called with 0 arguments) to name the comint buffer.
The final name will wrapped with \"*\"s. To have per-project
buffers, set this to
`hugging-chat-comint-default-shell-name-project-dedicated' (but
currently doesn't seem to work as the session is still shared)."
  :group 'hugging-chat-comint)

(defun hugging-chat-comint-shell-setup-buffer ()
  (visual-line-mode))

(defun hugging-chat-comint-shell--buffer (&optional wrap-astericks)
  (-->
   (cond
    ((functionp hugging-chat-comint-shell-name)
     (funcall hugging-chat-comint-shell-name))
    ((stringp hugging-chat-comint-shell-name)
     hugging-chat-comint-shell-name))
   (if wrap-astericks
       (concat "*" it "*")
     it)))

(defun hugging-chat-comint-read-model-name ()
  (completing-read
   "Hugging Chat model: "
   (lambda (string pred action)
     (if (eq action 'metadata)
         `(metadata (display-sort-function . identity))
       (complete-with-action
        action hugging-chat-comint-model-list string pred)))))

(defvar-local hugging-chat-comint--current-model nil)

;;;###autoload
(defun hugging-chat-comint-make-shell-buffer (&optional model comint-name)
  (-let* ((comint-name (or comint-name (hugging-chat-comint-shell--buffer)))
          (buf
           (apply #'make-comint
                  comint-name
                  hugging-chat-comint-script-path
                  nil
                  `(,@ (and model (list "--model" model))))))
    (with-current-buffer buf
      (unless hugging-chat-comint--current-model
        (setq-local hugging-chat-comint--current-model model))
      (hugging-chat-comint-shell-setup-buffer))
    buf))

;;;###autoload
(defun hugging-chat-comint-switch-to-shell-buffer (&optional model)
  "https://github.com/Soulter/hugging-chat-api.
Select MODEL to use."
  (interactive (list
                (cond
                 ((get-buffer (hugging-chat-comint-shell--buffer t))
                  nil)
                 (current-prefix-arg
                  (hugging-chat-comint-read-model-name))
                 (t
                  hugging-chat-comint-default-model))))
  (-let* ((buf-name (hugging-chat-comint-shell--buffer t)))
    (unless (get-buffer buf-name)
      (hugging-chat-comint-make-shell-buffer model))
    (pop-to-buffer buf-name)))

;;; hugging-chat-comint.el ends here

(provide 'hugging-chat-comint)
