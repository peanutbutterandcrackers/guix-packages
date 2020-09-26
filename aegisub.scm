(define-module (aegisub)
  #:use-module (srfi srfi-1)
  #:use-module (guix packages)
  #:use-module (guix store)
  #:use-module (guix derivations)
  #:use-module (guix build-system trivial)
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
      ;; instead add 'alsa-lib-with-pulseaudio-plugin' - a union
      ;; build of 'alsa-lib' and 'alsa-plugins:pulseaudio'
      ("alsa-lib-with-pulseaudio-plugin"
       ,(package
         (inherit alsa-lib)
         (name "alsa-lib-with-pulseaudio-plugin")
         (source #f)
         (build-system trivial-build-system)
         (arguments
          `(#:modules ((guix build union)
                       (guix build utils))
            #:builder
            (begin
              (use-modules (ice-9 match)
                           (guix build union)
                           (guix build utils))
              (let ((out (assoc-ref %outputs "out"))
                    (alsa-lib (assoc-ref %build-inputs "alsa-lib")))
                (match %build-inputs
                  (((names . directories) ...)
                   (union-build out
                                directories
                                ;; Instead of a symlink union, this is will be a
                                ;; copy union, because this won't "Just Work™"
                                ;; with mere symlinks. Some of the files that make
                                ;; up the union need to tweaked to work properly.
                                #:create-all-directories? #t
                                #:symlink copy-file)))
                ;; Fix alsa-lib pkg-config file to point to the /gnu/store of
                ;; this union, rather than that of alsa-lib itself.
                (substitute* (string-append out "/lib/pkgconfig/alsa.pc")
			     ((alsa-lib) out))
                #t))))
         (inputs
          `(("alsa-lib"
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
                                             )))))))))
            ("alsa-plugins:pulseaudio" ,alsa-plugins "pulseaudio"))))))
    )))
