(defsystem "example" 
  :class :package-inferred-system
  :author "Alexander Artemenko"
  :license "Unlicense"
  :pathname "src"
  :description "This description will be used only if long-description is missing"
  :long-description #.(uiop:read-file-string #P"docs/source/index.rst")
  :depends-on ("example/app"))


;; The code below does not work as expected, because
;; when you'll run (asdf:make "example/docs") the documentation
;; on packages will be missing. And asdf:make produces this strange errors:
;; Unknown error: #<ASDF/PACKAGE-INFERRED-SYSTEM:PACKAGE-INFERRED-SYSTEM "example"> is not a string designator.
;; Unknown error: :FORCE and :FORCE-NOT arguments not allowed in a nested call to ASDF/OPERATE:OPERATE unless identically to toplevel

(defclass build-docs-op (asdf:non-propagating-operation) ()
  (:documentation "Builds a documentations for the system."))


(defmethod asdf:perform ((o build-docs-op) (c asdf:system))
  (uiop:symbol-call :coo :document-system
                    (asdf:primary-system-name c)
                    :base-path (asdf/system:component-build-pathname c)))

(defsystem example/docs
  :defsystem-depends-on ("coo")
  :build-operation build-docs-op
  :build-pathname "docs/build/")
