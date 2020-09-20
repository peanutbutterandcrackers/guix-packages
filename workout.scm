(define-module (workout)
  #:use-module (guix packages)
  #:use-module (guix licenses)
  #:use-module (guix git-download)
  #:use-module (guix build-system copy)
  #:use-module (gnu packages speech))

(define-public workout
  (let ((ver "0")
        (rev "2")
        (commit "edee14300f0a8303c7344aac83cf73ef10c1fe05"))
    (package
     (name "workout")
     (version (git-version ver rev commit))
     (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/vlaube-de/7min-workout")
                    (commit commit)))
              (sha256
               (base32
                "07fiardk4z6wvnkcy1521f8asj6hmhhc678x8kwlqigs65p36l5q"))
              (modules '((guix build utils)))
              (snippet
               '(substitute* "workout.sh"
                             (("(echo \"\\$_str\" )(\\|)( espeak)" all echo pipe espeak)
                              (string-append echo pipe " tee /dev/tty " pipe espeak))))
              (file-name (git-file-name name version))))
     (build-system copy-build-system)
     (arguments
      `(#:install-plan
        '(("workout.sh" "bin/workout"))
        #:phases
        (modify-phases %standard-phases
                       (add-after 'install 'wrap-program
                                  (lambda* (#:key inputs outputs #:allow-other-keys)
                                           (let* ((out (assoc-ref outputs "out"))
                                                  (espeak (assoc-ref inputs "espeak"))
                                                  (prog (string-append
                                                         out "/bin/workout")))
                                             (wrap-program prog
                                                           `("PATH" ":" prefix
                                                             ,(list (string-append
                                                                     espeak "/bin" ":"
                                                                     (getenv "PATH"))))))
                                           #t)))))
     (inputs
      `(("espeak" ,espeak)))
     (synopsis "7 minute workout")
     (description "A simple shell script to help you with 7 minute workout.")
     (home-page "https://github.com/vlaube-de/7min-workout")
     (license expat))))
