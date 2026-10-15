(in-package #:autolith)

;;;; -- Cline Provider --

;;; Cline serves its whole roster at one Chat Completions endpoint and gates
;;; part of it to a request that carries the client identity below, so every
;;; request Autolith sends carries it. The identity mirrors the vendor's own
;;; client, including the platform name it reports, which is the name Node
;;; gives the host rather than Autolith's own.

(-> cline-client-platform-name () (option string))
(defun cline-client-platform-name ()
  "Return the platform name a Cline client reports for this host."
  (case (platform-host-family *platform*)
    (:windows "win32")
    (:macosx "darwin")
    (:linux "linux")))

(-> cline-client-headers () list)
(defun cline-client-headers ()
  "Return the Cline client identity every Cline request carries."
  (list (cons "HTTP-Referer" "https://cline.bot")
        (cons "X-TITLE" "Cline")
        (cons "X-IS-MULTIROOT" "false")
        (cons "X-CLIENT-TYPE" "cline-sdk")
        (cons "X-CLIENT-VERSION" *cline-client-version*)
        (cons "X-CORE-VERSION" *cline-client-core-version*)
        (cons "X-PLATFORM" (cline-client-platform-name))
        (cons "X-PLATFORM-VERSION" *cline-client-platform-version*)
        (cons "User-Agent" (format nil "Cline/~A" *cline-client-version*))))
