(define-module (slowmovideo)
  #:use-module (guix packages)
  #:use-module (gnu packages python)
  #:use-module (gnu packages perl)
  #:use-module (gnu packages qt)
  #:use-module (gnu packages video)
  #:use-module (gnu packages image-processing)
  #:use-module (guix git-download)
  #:use-module (guix build-system cmake)
  #:use-module (guix licenses))

(define-public slowmovideo
  (package
    (name "slowmovideo")
    (version "latest-git")
    (source
      (origin
  	  (method git-fetch)
  	  (uri (git-reference
  	       (url "https://github.com/slowmoVideo/slowmoVideo.git")
  		   (commit "6ed913bbf06758f10a41e44d8a8292ae555f486c")))
  	  (file-name (git-file-name name version))
  	  (sha256
  	    (base32
  		  "10w50cj05hbskfcpfzhmk4263vd8baks7k6y5iqrf78q43xx544n"))))
    (build-system cmake-build-system)
	(arguments
	  ;;<reepca> peanutbutterandc: from what I can tell they have a somewhat unusual build system for that project, as instead of the usual top-level invocation of cmake, they want you to run it in one of the two given subdirectories ("slowmoVideo" or "V3D"). So you'll need to replace the cmake-build-system's invocation of cmake (which currently passes "../source") with one that passes "../source/slowmoVideo". Alternatively, you could add a phase after
	  ;;<reepca> 'unpack which chdir's into slowmoVideo (source -> source/slowmoVideo) and assume that there's no existing directory named "build" in source, which is probably a safe assumption.
	  ;;<reepca> tl;dr version: (modify-phases %standard-phases (add-after 'unpack 'select-subproject (lambda _ (chdir "slowmoVideo"))))
	  `(#:tests? #f #:phases
	    (modify-phases %standard-phases
		  (add-after 'unpack 'select-subproject
		    (lambda _ (chdir "src"))))))
	(inputs
	  `(("ffmpeg" ,ffmpeg)
	    ("qtbase" ,qtbase)
	    ("qtscript" ,qtscript)
	    ("opencv" ,opencv)))
    (synopsis "Create slow-motion videos from your footage")
    (description synopsis)
    (home-page "http://slowmovideo.granjow.net/")
    (license gpl3)))

;; guix environment --pure
