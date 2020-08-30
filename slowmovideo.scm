(define-module (slowmovideo)
  #:use-module (guix packages)
  #:use-module (guix licenses)
  #:use-module (gnu packages qt)
  #:use-module (guix git-download)
  #:use-module (gnu packages video)
  #:use-module (gnu packages python)
  #:use-module (guix build-system cmake)
  #:use-module (gnu packages version-control)
  #:use-module (gnu packages image-processing))

(define-public slowmovideo
  (package
   (name "slowmovideo")
   (version "0.6")
   (source
    (origin
     (method git-fetch)
     (uri (git-reference
           (url "https://github.com/slowmoVideo/slowmoVideo.git")
           (commit (string-append "v" version))))
     (sha256
      (base32
       "0wgka5g248m17qmzdhzql17z6a046wxqv8zc6rar762f6g7qh3dr"))
     (file-name (git-file-name name version))))
   (build-system cmake-build-system)
   (arguments
    `(#:tests? #f ;; there are no tests
      #:phases
      (modify-phases %standard-phases
                     (add-after 'unpack 'add-submodules
                                (lambda _
                                  (copy-recursively (assoc-ref %build-inputs "libsvflow")
                                                    "src/lib/libsvflow")))
   (inputs
    `(("ffmpeg" ,ffmpeg)
      ("qtbase" ,qtbase)
      ("qtscript" ,qtscript)
      ("opencv" ,opencv)
      ("libsvflow"
       ;; This is a super-clever way to deal with git submodules
       ;; I was unable to deal with submodules using (git-reference
       ;; (recursive? #t). Partly because of git protocol used. This
       ;; clever method was suggested by leoprikler, who did this
       ;; same thing with `ppsspp`.
       ,(let ((commit "7c31a0bf9467e774442473e8b951b09fe6eb1b9f"))
          (origin
           (method git-fetch)
           (uri (git-reference
                 (url "https://github.com/slowmoVideo/libsvflow.git")
                 (commit commit)))
           (sha256
            (base32 "05q125350vg3q812p5ph0p14bbbicpw1dmisrxja4rvbi0lnixq1"))
           (file-name (git-file-name "libsvflow" commit)))))))
   (native-inputs
    `(("python-minimal" ,python-minimal)))
   (synopsis "Create slow-motion videos from your footage")
   (description "SlowmoVideo is a tool that uses optical flow for generating slow-motion videos.")
   (home-page "http://slowmovideo.granjow.net/")
   (license gpl3)))
