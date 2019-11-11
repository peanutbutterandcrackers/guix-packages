(define-module (imglapse)
  #:use-module (guix packages)
  #:use-module (guix git-download)
  #:use-module (guix build-system trivial)
  #:use-module (gnu packages base)
  #:use-module (gnu packages less)
  #:use-module (gnu packages bash)
  #:use-module (gnu packages video)
  #:use-module (guix licenses))

(define-public imglapse
  (package
  (name "imglapse")
  (version "c049a0260aa3294446757fa2fa02b61d99420a19")
  (source (origin
    (method git-fetch)
    (uri (git-reference
      (url "https://github.com/peanutbutterandcrackers/ImgLapse")
      (commit "c049a0260aa3294446757fa2fa02b61d99420a19")))
    (file-name (git-file-name name version))
    (sha256
      (base32
        "04k8fb6qzllk6i3vi7907hpm96sjbnacs607slsbz4rszgwc69jw"))))
  (build-system trivial-build-system)
  (arguments
    `(#:modules ((guix build utils))
      #:builder
        (begin
        (use-modules (guix build utils))
        (let* ((source (assoc-ref %build-inputs "source"))
               (in (string-append source "/imglapse.sh"))
               ;; move imglapse to a /bin subdir in /gnu/store/.../
               (out-dir (string-append (assoc-ref %outputs "out") "/bin"))
               ;; prepare to rename imglapse.sh to imglapse
               (out (string-append out-dir "/imglapse"))
               (bash (assoc-ref %build-inputs "bash"))
               (ffmpeg (assoc-ref %build-inputs "ffmpeg"))
               (less (assoc-ref %build-inputs "less"))
               (sed (assoc-ref %build-inputs "sed"))
               (coreutils (assoc-ref %build-inputs "coreutils")))
          (format #t "Copying ~a -> ~a~%" in out)
          ;; a '/bin' subdir is used because packages are merged to the profile
          ;; and thus, a package not in '/bin' but in it's parent dir will only
          ;; pollute the .guix-profile by residing at the profile root at merge
          (mkdir-p out-dir)
          (copy-file in out)
          ;; (setenv PATH ...) because (patch-shebang) [and (wrap-program) too]
          ;; expect bash in the PATH but trivial-build-system has an unset PATH
          ;; see `dehydrated in /gnu/packages/tls.scm for empty PATH patch/wrap
          (setenv "PATH" (string-append bash "/bin"))
          (patch-shebang out)
          ;; The following isn't inside (modify-phases) because the trivial-bu-
          ;; ild system does not have #:phases defined.
          (wrap-program out
            `("PATH" ":" prefix
             ,(map (lambda (dir)
               (string-append dir "/bin"))
                 (list ffmpeg less sed coreutils))))
          #t))))
  (inputs
    `(("bash" ,bash)
      ("less" ,less)
      ("ffmpeg" ,ffmpeg)
      ("sed" ,sed)
      ("coreutils" ,coreutils)))
  (synopsis "Create Timelapses on GNU/Linux")
  (description synopsis)
  (home-page "https://github.com/peanutbutterandcrackers/ImgLapse")
  (license gpl3)))
