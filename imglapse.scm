(define-module (imglapse)
  #:use-module (guix packages)
  #:use-module (guix licenses)
  #:use-module (guix git-download)
  #:use-module (guix build-system copy)
  #:use-module (gnu packages base)
  #:use-module (gnu packages less)
  #:use-module (gnu packages video))

(define-public imglapse
  (let ((ver "git-checkout")
        (rev "2")
        (commit "c049a0260aa3294446757fa2fa02b61d99420a19"))
    (package
     (name "imglapse")
     (version (git-version ver rev commit))
     (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/peanutbutterandcrackers/ImgLapse")
                    (commit "c049a0260aa3294446757fa2fa02b61d99420a19")))
              (sha256
               (base32
                "04k8fb6qzllk6i3vi7907hpm96sjbnacs607slsbz4rszgwc69jw"))
              (file-name (git-file-name name version))))
     (build-system copy-build-system)
     (arguments
      `(#:install-plan
        '(("imglapse.sh" "bin/imglapse"))
        #:phases
        (modify-phases %standard-phases
                       (add-after 'install 'wrap-program
                                  (lambda _
                                    (let* ((out (string-append
                                                 (assoc-ref %outputs "out")))
                                           (prog (string-append
                                                  out "/bin/imglapse")))
                                      (wrap-program prog
                                                    `("PATH" ":" suffix
                                                      ,(map (lambda (inpt)
                                                              (string-append
                                                               (assoc-ref
                                                                %build-inputs
                                                                inpt)
                                                               "/bin"))
                                                            '("ffmpeg"
                                                              "sed"
                                                              "coreutils"
                                                              "less")))))
                                    #t)))))
     (inputs
      `(("sed" ,sed)
        ("less" ,less)
        ("ffmpeg" ,ffmpeg)
        ("coreutils" ,coreutils)))
     (synopsis "Create Timelapses on GNU/Linux")
     (description synopsis)
     (home-page "https://github.com/peanutbutterandcrackers/ImgLapse")
     (license gpl3))))
