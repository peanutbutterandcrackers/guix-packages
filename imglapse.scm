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
					 (out-dir (string-append (assoc-ref %outputs "out") "/bin"))
					 (out (string-append out-dir "/imglapse")) ;; prep to rename 'imglapse.sh' to 'imglapse'
					 (bash (assoc-ref %build-inputs "bash"))
					 (ffmpeg (assoc-ref %build-inputs "ffmpeg"))
					 (less (assoc-ref %build-inputs "less"))
					 (coreutils (assoc-ref %build-inputs "coreutils")))
			    (format #t "Copying ~a -> ~a~%" in out)
				(mkdir-p out-dir) ;; create %outputs_out/bin. Why?
				;; <mange> When the package gets installed into a profile it effectively just gets merged into the profile at the root, which means the /bin directory goes to ~/.guix-profile/bin (in the default user profile). When you source ~/.guix-profile/etc/profile it adds ~/.guix-profile/bin to your PATH, which then lets you run the imglapse script as just "imglapse".
			    (copy-file in out)
                ;; the following doesn't work: might have to do (invoke (string-append coreutils "/bin/ls"))
				;;(invoke "ls" "-R")
				(substitute* out
				  (("/bin/bash")
				   (string-append bash "/bin/bash"))
				  (("ffmpeg")
				   (string-append ffmpeg "/bin/ffmpeg"))
				  (("cat")
				   (string-append coreutils "/bin/cat"))
				  (("echo")
				   (string-append coreutils "/bin/echo"))
				  (("dirname")
				   (string-append coreutils "/bin/dirname"))
				  (("basename")
				   (string-append coreutils "/bin/basename"))
				  (("less")
				   (string-append less "/bin/less"))))
				#t)))
	(inputs
		`(("bash" ,bash)
		  ("less" ,less)
		  ("ffmpeg" ,ffmpeg)
		  ("coreutils" ,coreutils)))
	(synopsis "Create Timelapses on GNU/Linux")
	(description synopsis)
	(home-page "https://github.com/peanutbutterandcrackers/ImgLapse")
	(license gpl3)))
