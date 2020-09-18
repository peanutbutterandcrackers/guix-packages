(define-module (workout)
  #:use-module (guix packages)
  #:use-module (guix licenses)
  #:use-module (guix git-download)
  #:use-module (guix build-system copy)
  #:use-module (gnu packages speech))

(define-public workout
  (let ((ver "0")
        (rev "1")
        (commit "9505051be0b728892cd130c68ee17d6481d5255c"))
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
                "05i9jz9kcbmkh8rbpmrr95w2dqy5nfrn9158i29zcxbgl594636g"))
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
     (license #f))))
