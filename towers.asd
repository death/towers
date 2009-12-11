;;;; +----------------------------------------------------------------+
;;;; | TOWERS - A silly geoDefense clone wannabe          DEATH, 2009 |
;;;; +----------------------------------------------------------------+

;;;; System definition

;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: CL-USER; Base: 10 -*-

(asdf:defsystem #:towers
  :depends-on (#:alexandria #:cl-opengl #:cl-glu #:cl-glut)
  :components ((:file "towers")))
