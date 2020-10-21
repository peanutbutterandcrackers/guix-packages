(define-module (trebleshot)
  #:use-module (guix packages)
  #:use-module (guix licenses)
  #:use-module (gnu packages qt)
  #:use-module (guix git-download)
  #:use-module (guix build-system cmake)
  #:use-module (gnu packages kde-frameworks))

(define-public trebleshot
  (let ((version "0.1.0-alpha2")
        (revision "1")
        (commit "ecf0ec654d08535047f92aeaa0474ceff93695ea"))
    (package
     (name "trebleshot")
     (version (git-version version revision commit))
     (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/trebleshot/desktop")
                    (commit commit)
                    (recursive? #t)))
              (sha256
               (base32
                "0bcrzhsir38civ3am86jrwvhhhrns133wz3gi9vbisjyifadnsbm"))
              (file-name (git-file-name name version))))
     (build-system cmake-build-system)
     (arguments
      `(#:tests? #f)) ;; no 'test' target available
     (inputs
      `(("qtbase" ,qtbase)
        ("kdnssd" ,kdnssd)))
     (synopsis "Desktop client for the FLOSS file-sharing app Trebleshot")
     (description "Trebleshot is an Open-Source Android application that allows
you to send and receive files over available connections.  It supports sharing
over HTTP, pausing and resuming transfers.  This package provides the desktop
client for the app.")
     (home-page "https://trebleshot.monora.org/")
     (license gpl2+))))
