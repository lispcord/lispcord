
(in-package lispcord-test)

(define-test class-definer-suite)

(defclass test-class ()
  ((slot :initarg :slot :accessor slotp)))

(defclass test-subclass (test-class)
  ((subslot :initarg :subslot :accessor subslot)))

(defmethod print-object ((o test-class) s)
  (print-unreadable-object (o s :type t :identity t)
    (format s "~S" (slotp o))))

(defmethod print-object ((o test-subclass) s)
  (print-unreadable-object (o s :type t :identity t)
    (format s "~S ~S" (slotp o) (subslot o))))

(defun test-equal (a b)
  (and (typep a 'test-class)
       (typep b 'test-class)
       (equal (slot-value a 'slot)
              (slot-value b 'slot))))

(defun subtest-equal (a b)
  (and (typep a 'test-subclass)
       (typep b 'test-subclass)
       (test-equal a b)
       (equal (slot-value a 'subslot)
              (slot-value b 'subslot))))

(define-test define-converters
  :parent class-definer-suite

  (is equalp
      '(slot identity identity)
      (lispcord.classes::make-converter 'slot))
  (is equalp
      '(slot parse-integer identity)
      (lispcord.classes::make-converter 'slot 'parse-integer))
  (is equalp
      '(slot parse-integer princ-to-string)
      (lispcord.classes::make-converter 'slot 'parse-integer 'princ-to-string))

  (true
   (lispcord.classes::define-converters (test-class) slot))
  (true
   (lispcord.classes::define-converters (test-subclass) (subslot))))

(defvar definer-json "{\"slot\":\"value\"}")
(defvar definer-table
  (alexandria:plist-hash-table '("slot" "value") :test 'equal))

(define-test to-json
  :parent class-definer-suite
  :depends-on (define-converters)
  (is string= definer-json
      (jonathan:to-json (make-instance 'test-class :slot "value"))))

(define-test from-json
  :parent class-definer-suite
  :depends-on (define-converters)
  (is test-equal (make-instance 'test-class :slot "value")
      (lispcord.classes:from-json 'test-class definer-table)))

(define-test update
  :parent class-definer-suite
  :depends-on (define-converters)
  (let ((obj1 (make-instance 'test-class :slot "other value"))
        (obj2 (make-instance 'test-class :slot "value")))
    (lispcord.classes::update definer-table obj1)
    (is test-equal obj2 obj1)))

(define-test unbound
  :parent class-definer-suite
  :depends-on (define-converters to-json from-json)
  (let ((obj (make-instance 'test-class)))
    (is test-equal (make-instance 'test-class :slot nil)
        (lispcord.classes::from-json 'test-class (lispcord.util:jparse (jonathan:to-json obj))))))

(define-test subclass
  :parent class-definer-suite
  :depends-on (define-converters to-json from-json)
  (let ((obj (make-instance 'test-subclass :slot "value" :subslot "subvalue")))
    (is subtest-equal obj
        (lispcord.classes:from-json 'test-subclass (lispcord.util:jparse (jonathan:to-json obj))))))
