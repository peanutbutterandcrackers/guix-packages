(define-module (chordpro)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module (guix build-system perl)
  #:use-module (guix licenses)
  #:use-module (gnu packages perl)
  #:use-module (gnu packages perl-check)
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
   (version "0.013")
   (source
    (origin
     (method url-fetch)
     (uri (string-append
	   "mirror://cpan/authors/id/J/JV/JV/Text-Layout-"
	   version
	   ".tar.gz"))
     (sha256
      (base32
       "02i5w42ypc9lbqvwqcsyzdnfy7xn7pvfql7lla1giwlzn4b0a8sw"))))
   (build-system perl-build-system)
   (native-inputs
    `(("perl-pdf-api2" ,perl-pdf-api2)))
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
		  (url "https://github.com/ChordPro/chordpro.git")
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
					   (wrap-program (string-append out "/bin/chordpro")
							 `("PERL5LIB" ":" prefix
							   ,(append
							     (map (lambda (input)
								    (string-append
								     (assoc-ref inputs input)
								     "/lib/perl5/site_perl"))
								  (list
								   "perl-cairo"
								   "perl-pango"
								   "perl-app-packager"
								   "perl-file-loadlines"
								   "perl-pdf-api2"
								   "perl-image-info"
								   "perl-string-interpolate-named"))
							     (list
							      (string-append out "/lib/perl5/site_perl"))))))
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
      ("perl-string-interpolate-named" ,perl-string-interpolate-named)))
   (synopsis "Simple text format for the notation of lyrics with chords")
   (description "Chordpro Dev Branch")
   (home-page "https://www.chordpro.org/")
   (license artistic2.0)))
