;;; conner.el --- define and run project specific commands  -*- lexical-binding:t -*-

;; Authors: Tomás Ralph <tomasralph2000@gmail.com>
;; Created: 2024
;; Version: 0.1
;; Package-Requires: ((emacs "27.1"))
;; Homepage: https://github.com/tralph3/conner
;; Keywords: tools


(defvar conner-file-name ".conner"
  "Filename where the launch commands will be defined.")

(setq-local conner--commands nil)

(defun conner--read-commands (root-dir)
  (let ((conner-file (file-name-concat root-dir conner-file-name)))
    (setq-local conner--commands
                (when (file-exists-p conner-file)
                  (with-temp-buffer
                    (insert-file-contents conner-file)
                    (read (current-buffer)))))))

(defun conner--write-commands (root-dir)
  (let ((conner-file (file-name-concat root-dir conner-file-name))
        (commands conner--commands))
    (with-temp-buffer
      (insert (pp commands))
      (write-file conner-file))))

(defun conner-run-project-command (&optional project)
  (interactive)
  (let ((project (or project (project-current t))))
    (conner-run-command (project-root project))))

(defun conner-add-project-command (&optional project)
  (interactive)
  (let ((project (or project (project-current t))))
    (conner-add-command (project-root project))))

(defun conner-delete-project-command (&optional project)
  (interactive)
  (let ((project (or project (project-current t))))
    (conner-delete-command (project-root project))))

(defun conner-run-command (root-dir)
  (interactive "D")
  (conner--read-commands root-dir)
  (let* ((names (mapcar #'car conner--commands))
         (command-name (completing-read "Select a command: " names))
         (command (cdr (assoc command-name conner--commands)))
         (default-directory root-dir))
    (compile command)))

(defun conner-add-command (root-dir)
  (interactive "D")
  (conner--read-commands root-dir)
  (let ((command-name (read-string "Enter command name: "))
        (command (read-string "Enter command: ")))
    (add-to-list 'conner--commands `(,command-name . ,command))
    (conner--write-commands root-dir)))

(defun conner-delete-command (root-dir)
  (interactive "D")
  (conner--read-commands root-dir)
  (let* ((names (mapcar #'car conner--commands))
         (command-name (completing-read "Delete command: " names))
         (element (assoc command-name conner--commands)))
    (setq-local conner--commands (delete element conner--commands))
    (conner--write-commands root-dir)))


(provide 'conner)

;;; conner.el ends here
