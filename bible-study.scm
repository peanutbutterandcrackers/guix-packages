(define-module (bible-study)
  #:use-module (guix packages)
  #:use-module (guix licenses)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module (guix build-system cmake)
  #:use-module (guix build-system gnu)
  #:use-module (gnu packages linux))

(define-public biblesync
  (package
   (name "biblesync")
   (version "2.1.0")
   (source (origin
            (method git-fetch)
            (uri (git-reference
                  (url "https://github.com/karlkleinpaste/biblesync")
                  (commit version)))
            (sha256
             (base32
              "0prmd12jq2cjdhsph5v89y38j7hhd51dr3r1hivgkhczr3m5hf4s"))
            (file-name (git-file-name name version))))
   (build-system cmake-build-system)
   (arguments
    `(#:tests? #f)) ;; no rule to make target 'test'
   (inputs
    `(("libuuid" ,util-linux "lib")))
   (synopsis "Multicast shared co-navigation library for Bible programs")
   (description "BibleSync is a multicast protocol to support Bible software
shared co-navigation.")
   (home-page "https://wiki.crosswire.org/BibleSync")
   (license public-domain)))

(define-public sword
  (package
   (name "sword")
   (version "1.8.0")
   (source (origin
            (method url-fetch)
            (uri (string-append "http://crosswire.org/ftpmirror/pub/sword/"
                                "source/v1.8/sword-1.8.0.tar.gz"))
            (sha256
             (base32
              "0mpry1shj98qc1l22ssfjzvgbn0j3vnl90x465wmrdf0qqiagsxm"))))
   (build-system gnu-build-system)
   (synopsis "API/library for bible software")
   (description "The SWORD Project is an open source, cross-platform API/library
for Bible software with a constantly growing list of front-ends (GUI, textmode,
web-based, etc.) and a library of over 200 text modules.")
   (home-page "http://www.crosswire.org/sword/")
   (license gpl2)))
