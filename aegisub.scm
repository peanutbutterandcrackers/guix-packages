(define-module (aegisub)
  #:use-module (guix utils)
  #:use-module (srfi srfi-1)
  #:use-module (guix packages)
  #:use-module (gnu packages video)
  #:use-module (gnu packages linux))

(define-public aegisub-custom
  (package
   (inherit aegisub)
   (inputs
    `(;; remove 'alsa-lib' from the inputs
      ,@(fold alist-delete
              (package-inputs aegisub)
              (list "alsa-lib"))
      ;; Add a custom build of alsa-lib which refernces the
      ;; correct /gnu/store-path of alsa-plugins:pulseaudio
      ("alsa-lib"
       ,(package
         (inherit alsa-lib)
         (arguments
          (substitute-keyword-arguments
           (package-arguments alsa-lib)
           ((#:configure-flags flags)
            `(cons (string-append
                    "--with-plugindir="
                    (assoc-ref %build-inputs "alsa-plugins:pulseaudio")
                    "/lib/alsa-lib") ,flags))))
         (inputs
          ;; Add alsa-plugins:pulseaudio as an input to alsa-lib itself
          `(("alsa-plugins:pulseaudio" ,alsa-plugins "pulseaudio")
            ,@(package-inputs alsa-lib)))))))))
