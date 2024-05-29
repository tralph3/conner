(defmacro with-temp-env (&rest forms)
  `(let* ((conner-file-name (concat conner-file-name (format "-test.%s" (% (random) 100000000))))
          (conner-root-dir "/tmp")
          (conner-env-file (concat conner-env-file (format "-test.%s" (% (random) 100000000))))
          (conner-file-path (file-name-concat conner-root-dir conner-file-name))
          (conner-env-path (file-name-concat conner-root-dir conner-env-file))
          (user-emacs-directory conner-root-dir)
          (conner-local-file-path (conner--construct-local-file-path "/tmp")))
     (unwind-protect
         (progn
           ,@forms)
       (delete-file conner-file-path)
       (delete-file conner-env-path)
       (delete-directory "/tmp/conner" t))))

(defun pollute-env-file ()
  (with-temp-buffer
    (insert "VAR1=someval")
    (write-file conner-env-path)))

(defun get-conner-contents ()
  (conner--read-commands conner-file-path))

(defun get-local-conner-contents ()
  (conner--read-commands conner-local-file-path))

(defun fake-command-runner (plist root-dir)
  (should (equal (plist-get plist :name) "Run me"))
  (should (equal (plist-get plist :command) "now"))
  (should (equal (plist-get plist :type) "test type"))
  (should (equal root-dir conner-root-dir)))

(defun fake-runner-check-env (plist &rest _)
  (if (equal (plist-get plist :command) "should exist")
      (should (equal (getenv "VAR1") "someval"))
    (should (equal (getenv "VAR1") nil))))

(defun fake-runner-check-default-dir (plist root-dir)
  (should (equal default-directory
                 (file-name-concat root-dir (plist-get plist :workdir)))))

(defun fake-func-check-display-buffer-alist (&optional is-silent)
  (let ((are-equal (equal (car display-buffer-alist) '("\\*conner-.*"
                                                       (display-buffer-no-window)
                                                       (allow-no-window . t)))))
    (if is-silent
        (should are-equal)
      (should-not are-equal))))

(ert-deftest conner-test-add-command ()
  (with-temp-env
   (conner-add-command conner-root-dir '(:name "New command" :command "echo \"test\"" :type "compile"))
   (should (equal (get-conner-contents)
                  '((:name "New command" :command "echo \"test\"" :type "compile"))))))

(ert-deftest conner-test-add-existing-command ()
  (with-temp-env
   (conner-add-command conner-root-dir '(:name "Test command" :command "echo \"test\"" :type "compile"))
   (should-error (conner-add-command conner-root-dir '(:name "Test command" :command "echo \"test\"" :type "compile")))))

(ert-deftest conner-test-add-invalid-command ()
  (with-temp-env
   (should-error
    (conner-add-command conner-root-dir '(:should "fail" :because "it" :lacks "name" :key)))))

(ert-deftest conner-test-delete-command ()
  (with-temp-env
   (conner-add-command conner-root-dir '(:name "Delete me" :command "please" :type "compile"))
   (conner-delete-command conner-root-dir "Delete me")
   (should (equal (get-conner-contents) nil))
   (conner-add-command conner-root-dir '(:name "Don't delete" :command "me" :type "compile"))
   (conner-add-command conner-root-dir '(:name "Do delete" :command "me" :type "compile"))
   (conner-delete-command conner-root-dir "Do delete")
   (should (equal (get-conner-contents) '((:name "Don't delete" :command "me" :type "compile"))))))

(ert-deftest conner-test-update-command ()
  (with-temp-env
   (conner-add-command conner-root-dir '(:name "Tpyo in nmae" :command "tpyo" :type "compile"))
   (conner-update-command conner-root-dir "Tpyo in nmae" '(:name "Typo in name" :command "typst" :type "compile"))
   (should (equal (get-conner-contents) '((:name "Typo in name" :command "typst" :type "compile"))))))

(ert-deftest conner-test-run-command ()
  (with-temp-env
   (let ((conner-command-types-alist `(("test type" ,#'fake-command-runner))))
     (conner-add-command conner-root-dir '(:name "Run me" :command "now" :type "test type"))
     (conner-run-command conner-root-dir "Run me"))))

(ert-deftest conner-test-construct-file-path ()
  (with-temp-env
   (should (equal (conner--construct-file-path conner-root-dir)
                  (file-name-concat conner-root-dir conner-file-name)))))

(ert-deftest conner-test-construct-local-file-path ()
  (with-temp-env
   (should (equal (conner--construct-local-file-path conner-root-dir)
                  (expand-file-name (file-name-concat user-emacs-directory "conner/!tmp.#conner#"))))))

(ert-deftest conner-test-add-local-command ()
  (with-temp-env
   (let ((current-prefix-arg 4))
     (conner-add-command conner-root-dir '(:name "New command" :command "echo \"test\"" :type "compile"))
     (should (file-exists-p conner-local-file-path))
     (should-not (file-exists-p conner-file-path)))))

(ert-deftest conner-test-delete-local-command ()
  (with-temp-env
   (let ((current-prefix-arg 4))
     (conner-add-command conner-root-dir '(:name "New command" :command "echo \"test\"" :type "compile"))
     (conner-delete-command conner-root-dir "New command")
     (should (file-exists-p conner-local-file-path))
     (should-not (file-exists-p conner-file-path))
     (should (equal (get-local-conner-contents) nil)))))

(ert-deftest conner-test-update-local-command ()
  (with-temp-env
   (let ((current-prefix-arg 4))
     (conner-update-command conner-root-dir "Old command" '(:name "New command" :command "echo \"test\"" :type "compile"))
     (should (file-exists-p conner-local-file-path))
     (should-not (file-exists-p conner-file-path))
     (should (equal (get-local-conner-contents)
                    '((:name "New command" :command "echo \"test\"" :type "compile")))))))

(ert-deftest conner-test-run-local-command ()
  (with-temp-env
   (let ((current-prefix-arg 4)
         (conner-command-types-alist `(("test type" ,#'fake-command-runner))))
     (conner-add-command conner-root-dir '(:command "now" :name "Run me" :type "test type"))
     (conner-run-command conner-root-dir "Run me"))))

(ert-deftest conner-test-read-env-file ()
  (with-temp-env
   (pollute-env-file)
   (let ((conner-command-types-alist `(("test type" ,#'fake-runner-check-env))))
     (conner-add-command conner-root-dir '(:name "Run me" :command "should exist" :type "test type"))
     (conner-run-command conner-root-dir "Run me"))))

(ert-deftest conner-test-dont-read-env-file ()
  (with-temp-env
   (pollute-env-file)
   (let ((conner-command-types-alist `(("test type" ,#'fake-runner-check-env)))
         (conner-read-env-file nil))
     (conner-add-command conner-root-dir '(:name "Run me" :command "echo $VAR1" :type "test type"))
     (conner-run-command conner-root-dir "Run me"))))

(ert-deftest conner-test-add-local-command-with-default-behavior ()
  (with-temp-env
   (let ((current-prefix-arg 4)
         (conner-default-file-behavior 'local))
     (conner-add-command conner-root-dir '(:name "New command" :command "echo \"test\"" :type "compile"))
     (should-not (file-exists-p conner-local-file-path))
     (should (file-exists-p conner-file-path)))))

(ert-deftest conner-test-delete-local-command-with-default-behavior ()
  (with-temp-env
   (let ((current-prefix-arg 4)
         (conner-default-file-behavior 'local))
     (conner-add-command conner-root-dir '(:name "New command" :command "echo \"test\"" :type "compile"))
     (conner-delete-command conner-root-dir "New command")
     (should-not (file-exists-p conner-local-file-path))
     (should (file-exists-p conner-file-path))
     (should (equal (get-conner-contents) nil)))))

(ert-deftest conner-test-update-local-command-with-default-behavior ()
  (with-temp-env
   (let ((current-prefix-arg 4)
         (conner-default-file-behavior 'local))
     (conner-update-command conner-root-dir "Old command" '(:name "New command" :command "echo \"test\"" :type "compile"))
     (should-not (file-exists-p conner-local-file-path))
     (should (file-exists-p conner-file-path))
     (should (equal (get-conner-contents) '((:name "New command" :command "echo \"test\"" :type "compile")))))))

(ert-deftest conner-test-run-local-command-with-default-behavior ()
  (with-temp-env
   (let ((current-prefix-arg 4)
         (conner-default-file-behavior 'local)
         (conner-command-types-alist `(("test type" ,#'fake-command-runner))))
     (conner-add-command conner-root-dir '(:name "Run me" :command "now" :type "test type"))
     (conner-run-command conner-root-dir "Run me"))))

(ert-deftest conner-test-read-command-env-var ()
  (with-temp-env
   (let ((conner-command-types-alist `(("test type" ,#'fake-runner-check-env))))
     (conner-add-command conner-root-dir '(:name "Run me" :command "should exist" :type "test type" :environment ("VAR1: someval")))
     (conner-run-command conner-root-dir "Run me"))))

(ert-deftest conner-test-change-workdir ()
  (with-temp-env
   (let ((conner-command-types-alist `(("workdirchange" ,#'fake-runner-check-default-dir))))
     (conner-add-command conner-root-dir '(:name "Run me" :workdir "/relative/to/rootdir" :command "now" :type "workdirchange"))
     (conner-run-command conner-root-dir "Run me"))))

(ert-deftest conner-test-expand-command ()
  (with-temp-env
   (let ((default-directory "/test/path"))
     (should (equal (conner-expand-command "Expand: %d. This does not: %%d. Escape percent: %%")
                    "Expand: /test/path. This does not: %d. Escape percent: %"))
     (should-error (conner-expand-command "Non-existent specifier %z")))))

(ert-deftest conner-test-validate-command ()
  (with-temp-env
   (should-error (conner--validate-command-plist '("Not" "a" "plist")))
   (should-error (conner--validate-command-plist '(:name notstring :command "test" :type "compile")))
   (should-error (conner--validate-command-plist '(:name "name" :type "compile")))
   (should-error (conner--validate-command-plist '(:name "name" :command "test" :type notstring)))
   (should-error (conner--validate-command-plist '(:name "name" :command "test" :type "invalidtype")))
   (should-error (conner--validate-command-plist '(:name "name" :command "test" :type "compile" :workdir notstring)))
   (should-error (conner--validate-command-plist '(:name "name" :command "test" :type "compile" :environment notlist)))
   (should-error (conner--validate-command-plist '(:name "name" :command "test" :type "compile" :hook "not a symbol!")))
   (should-error (conner--validate-command-plist '(:name "name" :command "test" :type "compile" :silent "not a boolean")))
   (should (eq (conner--validate-command-plist '(:name "name" :command symbol :type "compile")) nil))
   (should (eq (conner--validate-command-plist '(:name "name" :command "test" :type "compile")) nil))))

(ert-deftest conner-test-hooks ()
  (with-temp-env
   (setq-local hook-test-value 42)
   (defun func-hook-test ()
     (setq-local hook-test-value 69))
   (conner-add-command conner-root-dir '(:name "Run me" :command "now" :type "compile" :hook func-hook-test))
   (conner-run-command conner-root-dir "Run me")
   (should (eq 69 hook-test-value))))

(ert-deftest conner-test-clean-command ()
  (with-temp-env
   (should (equal (conner--clean-command-plist '(:hook nil :name "some command" :workdir nil)) '(:name "some command")))))

(ert-deftest conner-test-silent-command ()
  (with-temp-env
   (conner-add-command conner-root-dir '(:name "Silent command" :command (lambda () (fake-func-check-display-buffer-alist t)) :type "elispf" :silent t))
   (conner-add-command conner-root-dir '(:name "Normal command" :command fake-func-check-display-buffer-alist :type "elispf"))
   (conner-run-command conner-root-dir "Silent command")
   (conner-run-command conner-root-dir "Normal command")))

(ert-deftest conner-test-project-backend ()
  (with-temp-env
   (let ((conner-project-backend 'projectile))
     (defun projectile-project-root (root-dir)
       root-dir)
     (defun projectile-relevant-known-projects ())
     (conner--act-on-project (lambda (root-dir) (should (equal root-dir "/test"))) "/test"))
   (let ((conner-project-backend 'project.el))
     (conner--act-on-project (lambda (root-dir) (should (equal root-dir "/test"))) '(vc Git "/test")))))
