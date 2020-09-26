(define-module (aegisub)
  #:use-module (srfi srfi-1)
  #:use-module (guix packages)
  #:use-module (guix store)
  #:use-module (guix derivations)
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
         (arguments `(,@(cons
                         (car (package-arguments alsa-lib))
                         (list (append
                                (cadr
                                 (package-arguments alsa-lib))
                                (list (string-append
                                       "--with-plugindir="
                                       ;; set plugin dir to be the "lib/alsa-lib"
                                       ;; directory of the "pulseaudio" output of
                                       ;; what will become the /gnu/store path of
                                       ;; alsa-plugins:pulseaudio.
                                       (derivation-output-path
                                        (assoc-ref
                                         (derivation-outputs
                                          ((package->derivation alsa-plugins)
                                           (open-connection)))
                                         "pulseaudio"))
				       "/lib/alsa-lib")
                                       ))))))))
      ;; alsa-plugins:pulseaudio, of course
      ("alsa-plugins:pulseaudio" ,alsa-plugins "pulseaudio")))))
