(define-module (chordpro)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module (guix build-system perl)
  #:use-module (guix licenses)
  #:use-module (gnu packages perl)
  #:use-module (gnu packages perl-check)
  #:use-module (gnu packages python)
  #:use-module (gnu packages glib)
  #:use-module (gnu packages gtk))

(define perl-app-packager
  (package
   (name "perl-app-packager")
   (version "1.430.1")
   (source
    (origin
     (method url-fetch)
     (uri (string-append
           "mirror://cpan/authors/id/J/JV/JV/App-Packager-"
           version
           ".tar.gz"))
     (sha256
      (base32
       "18vlwf5qsmw64kr0mc84cn55nm72s4aqdz9dxpigk1w38lad1x2p"))))
   (build-system perl-build-system)
   (home-page
    "https://metacpan.org/release/App-Packager")
   (synopsis "Abstraction for Packagers")
   (description "Abstraction for Packagers")
   (license perl-license)))

(define perl-file-loadlines
  (package
   (name "perl-file-loadlines")
   (version "0.02")
   (source
    (origin
     (method url-fetch)
     (uri (string-append
           "mirror://cpan/authors/id/J/JV/JV/File-LoadLines-"
           version
           ".tar.gz"))
     (sha256
      (base32
       "0yvbwj5mi83f95zrlyb0dga66ds8yyc70w50r79lssbvrwqiq35b"))))
   (build-system perl-build-system)
   (home-page
    "https://metacpan.org/release/File-LoadLines")
   (synopsis "Load lines from file")
   (description "Load lines from file")
   (license perl-license)))

(define perl-image-info
  (package
   (name "perl-image-info")
   (version "1.42")
   (source
    (origin
     (method url-fetch)
     (uri (string-append
           "mirror://cpan/authors/id/S/SR/SREZIC/Image-Info-"
           version
           ".tar.gz"))
     (sha256
      (base32
       "0kwg9a8in219p4ka1ykzdfcfpfpkc979ffmccg6w3cvi7w65djib"))))
   (build-system perl-build-system)
   (propagated-inputs
    `(("perl-io-stringy" ,perl-io-stringy)))
   (home-page
    "https://metacpan.org/release/Image-Info")
   (synopsis
    "Extract meta information from image files")
   (description "Extract meta information from image files")
   (license perl-license)))

(define perl-string-interpolate-named
  (package
   (name "perl-string-interpolate-named")
   (version "0.05")
   (source
    (origin
     (method url-fetch)
     (uri (string-append
           "mirror://cpan/authors/id/J/JV/JV/String-Interpolate-Named-"
           version
           ".tar.gz"))
     (sha256
      (base32
       "106mkhkd1x463dczjcs0hv4k7604xdd0hlwpy8yhk5gfbiirxjc4"))))
   (build-system perl-build-system)
   (home-page
    "https://metacpan.org/release/String-Interpolate-Named")
   (synopsis
    "Interpolated named arguments in string")
   (description "Interpolated named arguments in string")
   (license perl-license)))

(define perl-text-layout
  (package
   (name "perl-text-layout")
   (version "0.019")
   (source
    (origin
     (method url-fetch)
     (uri (string-append
           "mirror://cpan/authors/id/J/JV/JV/Text-Layout-"
           version
           ".tar.gz"))
     (sha256
      (base32
       "13hhlka8vl1q3f8968a8vmgdg3irm0gsgvx94g2jjfqikslg4hx0"))))
   (build-system perl-build-system)
   (native-inputs
    `(("perl-pdf-api2" ,perl-pdf-api2)))
   (propagated-inputs
    `(("perl-harfbuzz-shaper" ,perl-harfbuzz-shaper)))
   (home-page
    "https://metacpan.org/release/Text-Layout")
   (synopsis "Pango style markup formatting")
   (description synopsis)
   (license perl-license)))

(define-public chordpro
  (package
   (name "chordpro")
   (version "0.974_017")
   (source (origin
            (method git-fetch)
            (uri (git-reference
                  (url "https://github.com/ChordPro/chordpro")
                  (commit "03c72e8e651bca0f24ee6d8b59e61750fe10f8b7")))
            (file-name (git-file-name name version))
            (sha256
             (base32
              "14770w94p1granp1b8f2bra1kf44xxv5gklm3ccmf1qs9pgy5wxp"))))
   (build-system perl-build-system)
   (arguments
    `(#:phases
      (modify-phases %standard-phases
                     (add-after 'install 'wrap
                                (lambda* (#:key inputs outputs #:allow-other-keys)
                                         (let* ((out (assoc-ref outputs "out")))
                                           (wrap-program
                                            (string-append
                                             out
                                             "/bin/chordpro")
                                            `("PERL5LIB" ":" =
                                              (,(getenv "PERL5LIB")
                                               ,(string-append
                                                 out "/lib/perl5/site_perl")))))
                                         #t)))))
   (native-inputs
    `(("perl-cpan-changes" ,perl-cpan-changes)
      ("perl-text-layout"  ,perl-text-layout)))
   (inputs
    `(("perl-cairo" ,perl-cairo)
      ("perl-pango" ,perl-pango)
      ("perl-app-packager" ,perl-app-packager)
      ("perl-file-loadlines" ,perl-file-loadlines)
      ("perl-pdf-api2" ,perl-pdf-api2)
      ("perl-image-info" ,perl-image-info)
      ("perl-string-interpolate-named" ,perl-string-interpolate-named)
      ;; The following are the-missing-links: propagated-inputs of the
      ;; direct/first dependencies of chordpro. The adjacent comments
      ;; tell which dependency they are a propagated-input of.
      ("perl-glib" ,perl-glib) ;; perl-cairo
      ("perl-font-ttf" ,perl-font-ttf) ;; perl-pdf-api2
      ("perl-io-stringy" ,perl-io-stringy))) ;; perl-image-info
   (synopsis "Simple text format for the notation of lyrics with chords")
   (description "Chordpro Dev Branch")
   (home-page "https://www.chordpro.org/")
   (license artistic2.0)))

(define perl-harfbuzz-shaper
  (package
   (name "perl-harfbuzz-shaper")
   (version "0.023")
   (source
    (origin
     (method url-fetch)
     (uri (string-append
           "mirror://cpan/authors/id/J/JV/JV/HarfBuzz-Shaper-"
           version
           ".tar.gz"))
     (sha256
      (base32
       "0kcyyvcppk1h3fq9vmndiks6vzyr965vpzfja8p8pymm430616qr"))))
   (build-system perl-build-system)
   (arguments
    ;; See https://github.com/sciurius/perl-HarfBuzz-Shaper/issues/8
    `(#:make-maker-flags (list (string-append
                                "LIBS=-L"
                                (assoc-ref %build-inputs "harfbuzz")
                                "/lib"
                                " -lharfbuzz"))))
   (native-inputs
    `(("python-2" ,python-2)))
   (inputs
    `(("harfbuzz" ,harfbuzz)))
   (home-page
    "https://metacpan.org/release/HarfBuzz-Shaper")
   (synopsis "Use HarfBuzz for text shaping")
   (description "HarfBuzz::Shaper is a perl module that provides access to a
small subset of the native HarfBuzz library. The subset is suitable for typesetting
programs that need to deal with complex languages like Devanagari, Hebrew or Arabic.")
   (license perl-license)))

(define-public chordpro-next
  (package
   (inherit chordpro)
   (name "chordpro-next")
   (version "0.977")
   (source (origin
            (method git-fetch)
            (uri (git-reference
                  (url "https://github.com/ChordPro/chordpro")
                  (commit "c295821a0d047a2afe4370f444bff2cb3f7fbacb")))
            (file-name (git-file-name name version))
            (sha256
             (base32
              "1im7r5akf86ivyd41wr907br27lghr73c8f62bk7rv83529aczbb"))))
   (inputs
    `(("perl-app-packager" ,perl-app-packager)
      ("perl-file-loadlines" ,perl-file-loadlines)
      ("perl-pdf-api2" ,perl-pdf-api2)
      ("perl-image-info" ,perl-image-info)
      ("perl-string-interpolate-named" ,perl-string-interpolate-named)
      ("perl-text-layout" ,perl-text-layout)
      ;; The following are the-missing-links: propagated-inputs of the
      ;; direct/first dependencies of chordpro. The adjacent comments
      ;; tell which dependency they are a propagated-input of.
      ("perl-font-ttf" ,perl-font-ttf) ;; perl-pdf-api2
      ("perl-harfbuzz-shaper" ,perl-harfbuzz-shaper) ;; perl-text-layout
      ("perl-io-stringy" ,perl-io-stringy))) ;; perl-image-info
   (synopsis "Simple text format for the notation of lyrics with chords")
   (description "Chordpro Dev Branch")
   (home-page "https://www.chordpro.org/")
   (license artistic2.0)))
