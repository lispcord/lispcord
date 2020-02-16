
(in-package lispcord-tests)

(define-test class-definer-suite)

(defclass test-class ()
  ((slot :initarg :slot :accessor slotp)))

(defmethod print-object ((o test-class) s)
  (print-unreadable-object (o s :type t :identity t)
    (format s "~S" (slotp o))))

(print-object )

(defun test-equal (a b)
  (and (typep a 'test-class)
       (typep b 'test-class)
       (equal (slot-value a 'slot)
              (slot-value b 'slot))))

(define-test define-converters
  :parent class-definer-suite

  (is equalp '(slot identity nil)
      (lispcord.classes::make-converter 'slot))
  (is equalp '(slot parse-integer nil)
      (lispcord.classes::make-converter 'slot 'parse-integer))
  (is equalp '(slot parse-integer 0)
      (lispcord.classes::make-converter 'slot 'parse-integer 0))

  (true
   (lispcord.classes::define-converters (test-class)
     (slot))))

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
