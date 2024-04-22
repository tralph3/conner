(defmacro with-temp-env (&rest forms)
  `(let* ((conner-file-name (concat conner-file-name (format "-test.%s" (% (random) 100000000))))
          (conner-root-dir "/tmp")
          (conner-file-path (file-name-concat conner-root-dir conner-file-name)))
     ,@forms
     (delete-file conner-file-path)))

(defun get-conner-contents ()
  (conner--read-commands conner-file-path))

(defun fake-command-runner (command element root-dir)
  (should (equal command "now"))
  (should (equal element '("Run me" "now" "test type")))
  (should (equal root-dir conner-root-dir)))

(ert-deftest conner-test-add-command ()
  (with-temp-env (conner-add-command conner-root-dir "New command" "echo \"test\"" "compile")
                 (should (equal (get-conner-contents)
                                '(("New command" "echo \"test\"" "compile"))))))

(ert-deftest conner-test-add-existing-command ()
  (with-temp-env (conner-add-command conner-root-dir "Test command" "yes test" "compile")
                 (should-error (conner-add-command conner-root-dir "Test command" "other stuff" "compile"))))

(ert-deftest conner-test-add-with-default-command-type ()
  (with-temp-env (let ((conner-command-types-alist `(("new type" ,#'car)))
                       (conner-default-command-type "new type"))
                   (conner-add-command conner-root-dir "Some" "command")
                   (should (equal (get-conner-contents) '(("Some" "command" "new type")))))))

(ert-deftest conner-test-delete-command ()
  (with-temp-env (conner-add-command conner-root-dir "Delete me" "please" "compile")
                 (conner-delete-command conner-root-dir "Delete me")
                 (should (equal (get-conner-contents) nil))
                 (conner-add-command conner-root-dir "Don't delete" "me" "compile")
                 (conner-add-command conner-root-dir "Do delete" "me" "compile")
                 (conner-delete-command conner-root-dir "Do delete")
                 (should (equal (get-conner-contents) '(("Don't delete" "me" "compile"))))))

(ert-deftest conner-test-update-command ()
  (with-temp-env (conner-add-command conner-root-dir "Tpyo in nmae" "tpyo" "comple")
                 (conner-update-command conner-root-dir "Tpyo in nmae" "Typo in name" "typst" "compile")
                 (should (equal (get-conner-contents) '(("Typo in name" "typst" "compile"))))))

(ert-deftest conner-test-run-command ()
  (with-temp-env (let ((conner-command-types-alist `(("test type" ,#'fake-command-runner))))
                   (conner-add-command conner-root-dir "Run me" "now" "test type")
                   (conner-run-command conner-root-dir "Run me"))))

(ert-deftest conner-test-construct-file-path ()
  (with-temp-env (should (equal (conner--construct-file-path conner-root-dir)
                                (file-name-concat conner-root-dir conner-file-name)))))

(ert-deftest conner-test-construct-local-file-path ()
  (with-temp-env (should (equal (conner--construct-local-file-path conner-root-dir)
                                (expand-file-name (file-name-concat user-emacs-directory "conner/!tmp.#conner"))))))
