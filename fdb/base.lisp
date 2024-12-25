(in-package :fdb)

(use-foreign-library libfdb)

(defvar *foreign-encoding* :utf-8)

;;; Configuration
(defvar *api-version* 630
  "The API version to use when connecting to FoundationDB.")

(defvar *cluster-file* "/etc/foundationdb/fdb.cluster"
  "Default path to the cluster file.")

(defvar *db* nil
  "The current database connection.")

;;; Error Handling

(define-condition fdb-error (error)
  ((code :initarg :code :reader fdb-error-code)
   (message :initarg :message :reader fdb-error-message))
  (:report (lambda (condition stream)
             (format stream "FoundationDB error ~A: ~A"
                     (fdb-error-code condition)
                     (fdb-error-message condition)))))

(defun check-error (error-code)
  "Checks the given FDB error code and signals an fdb-error condition if it's not zero."
  (unless (zerop error-code)
    (let ((message (fdb-get-error error-code)))
      (error 'fdb-error :code error-code :message message))))

(defun select-api-version (version)
  "Selects the version of the FoundationDB API to use.
Must be called before any other functions."
  (check-error (fdb-select-api-version-impl version *api-version*)))

(defun setup-network ()
  "Sets up the FDB network. Call after select-api-version."
  (check-error (fdb-setup-network)))

(defun run-network ()
  "Starts the FDB network event loop. Must be run in a separate thread."
  (check-error (fdb-run-network)))

(defun start-server ()
  "Starts the FoundationDB network thread."
  (select-api-version *api-version*)
  (setup-network)
  (sb-thread:make-thread (lambda () (run-network)) :name "fdb-network-thread")
  (connect-db))

(defun stop-network ()
  "Stops the FDB network event loop."
  (check-error (fdb-stop-network)))

(defun connect-db (&optional (cluster-file-path *cluster-file*))
  "Connects to a FoundationDB database using the given cluster file path.
Returns the database object."
  (let ((db-ptr (foreign-alloc :pointer)))
    (check-error (fdb-create-database cluster-file-path db-ptr))
    (setf *db* (mem-ref db-ptr :pointer))
    *db*))

(defun close-db ()
  "Closes the current database connection."
  (when *db*
    (fdb-database-destroy *db*)
    (setf *db* nil)))

(defun database-create-transaction (database)
  "Creates a new transaction object. Internal use, prefer with-transaction."
  (let ((tr-ptr (foreign-alloc :pointer)))
    (check-error (fdb-database-create-transaction database tr-ptr))
    (mem-ref tr-ptr :pointer)))

(defun transaction-set (transaction key value)
  "Sets the value of a key in the given transaction."
  (let* ((key-pointer (foreign-string-alloc key))
	 (key-len (foreign-funcall "strlen" :pointer key-pointer :int))
	 (value-pointer (foreign-string-alloc (format nil "~a" value)))
	 (value-len (foreign-funcall "strlen" :pointer value-pointer :int)))
    (fdb-transaction-set transaction key-pointer key-len value-pointer value-len)
    (foreign-string-free key-pointer)
    (foreign-string-free value-pointer)))

(defun transaction-clear (transaction key)
  "Clears a key in the given transaction."
  (with-foreign-string ((c-key key-len) key)
    (fdb-transaction-clear transaction c-key key-len)))

(defun transaction-clear-range (transaction begin-key end-key)
  "Clears a range of keys in the given transaction."
  (with-foreign-string ((c-begin-key begin-key-len) begin-key)
    (with-foreign-string ((c-end-key end-key-len) end-key)
      (fdb-transaction-clear-range transaction c-begin-key begin-key-len c-end-key end-key-len))))

(defun transaction-atomic-op (transaction key param operation-type)
  "Performs an atomic operation on a key in the given transaction."
  (with-foreign-string ((c-key key-len) key)
    (with-foreign-string ((c-param param-len) param)
      (fdb-transaction-atomic-op transaction c-key key-len c-param param-len operation-type))))

(defun block-and-get-value (future get-value-fn)
  "Blocks until the future is ready, then extracts the value using the provided function."
  (check-error (fdb-future-block-until-ready future))
  (let ((err-code (fdb-future-get-error future)))
    (if (zerop err-code)
        (with-foreign-objects ((value-ptr :pointer)
                               (length-ptr :int)
                               (present-ptr :boolean))
          (check-error (funcall get-value-fn future present-ptr value-ptr length-ptr))
          (let* ((present (if (plusp (mem-ref present-ptr 'fdb-bool-t)) t nil))
                 (value (if present
                            (foreign-string-to-lisp (mem-ref value-ptr :pointer) :count (mem-ref length-ptr :int))
                            nil)))
            (values value present)))
        (values nil nil))))

(defun transaction-get (transaction key &optional (snapshot nil))
  "Gets the value associated with a key in a transaction.
Returns the value and a boolean indicating if the key was present."
  (let* ((*key (foreign-string-alloc key))
	 (key-len (foreign-funcall "strlen" :pointer *key :int))
	 (future (fdb-transaction-get transaction *key key-len (convert-to-foreign snapshot :boolean))))
    (multiple-value-bind (value present) (block-and-get-value future #'fdb-future-get-value)
      (fdb-future-destroy future)
      (values value present))))

(defun block-and-get-int64 (future)
  "Blocks until the future is ready, then extracts an int64 value."
  (check-error (fdb-future-block-until-ready future))
  (with-foreign-object (int-ptr :int64)
    (check-error (fdb-future-get-int64 future int-ptr))
    (mem-ref int-ptr :int64)))

(defun block-and-get-key (future)
  "Blocks until the future is ready, then extracts a key (byte string)."
  (check-error (fdb-future-block-until-ready future))
  (with-foreign-objects ((key-ptr :pointer)
                         (length-ptr :int))
    (check-error (fdb-future-get-key future key-ptr length-ptr))
    (foreign-string-to-lisp (mem-ref key-ptr :pointer) :count (mem-ref length-ptr :int))))

(defun block-and-get-keyvalue-array (future)
  "Blocks until the future is ready, then extracts an array of key-value pairs."
  (check-error (fdb-future-block-until-ready future))
  (with-foreign-objects ((kv-ptr :pointer)
                         (count-ptr :pointer)
                         (more-ptr :pointer))
    (check-error (fdb-future-get-keyvalue-array future kv-ptr count-ptr more-ptr))
    (let* ((count (mem-ref count-ptr :int))
           (more (if (plusp (mem-ref more-ptr 'fdb-bool-t)) t nil))
           (kv-array (loop for i below count
                           for kv-struct = (mem-aptr (mem-ref kv-ptr :pointer) '(:struct fdb-key-value) i)
                           collect (cons (foreign-string-to-lisp (foreign-slot-value kv-struct '(:struct fdb-key-value) 'key)
                                                                 :count (foreign-slot-value kv-struct '(:struct fdb-key-value) 'key-length))
                                         (foreign-string-to-lisp (foreign-slot-value kv-struct '(:struct fdb-key-value) 'value)
                                                                 :count (foreign-slot-value kv-struct '(:struct fdb-key-value) 'value-length))))))
      (values kv-array more))))

(defun block-and-get-key-array (future)
  "Blocks until the future is ready, then extracts an array of keys."
  (check-error (fdb-future-block-until-ready future))
  (with-foreign-objects ((key-array-ptr :pointer)
                         (count-ptr :int))
    (check-error (fdb-future-get-key-array future key-array-ptr count-ptr))
    (let* ((count (mem-ref count-ptr :int))
           (key-array (loop for i below count
                            for key = (mem-aptr (mem-ref key-array-ptr :pointer) :pointer i)
                            collect (foreign-string-to-lisp key))))
      key-array)))

(defun transaction-get-range (transaction begin-key end-key &key (limit 0) (target-bytes 0) (mode :iterator) (iteration 0) (snapshot nil) (reverse nil) (begin-or-equal t) (begin-offset 0) (end-or-equal t) (end-offset 0))
  "Gets a range of key-value pairs from a transaction.
   Returns a list of (key . value) pairs and a boolean indicating if more data is available."
  (let* ((*begin-key (foreign-string-alloc begin-key))
	 (begin-key-len (foreign-funcall "strlen" :pointer *begin-key :int))
	 (*end-key (foreign-string-alloc end-key))
	 (end-key-len (foreign-funcall "strlen" :pointer *end-key :int))
	 (future (fdb-transaction-get-range transaction
					    *begin-key
					    begin-key-len
					    (convert-to-foreign begin-or-equal :boolean)
					    begin-offset
					    *end-key end-key-len
					    (convert-to-foreign end-or-equal :boolean)
					    end-offset
					    limit
					    target-bytes
					    (ecase mode
					      (:iterator :fdb-streaming-mode-iterator)
					      (:small :fdb-streaming-mode-small)
					      (:medium :fdb-streaming-mode-medium)
					      (:large :fdb-streaming-mode-large)
					      (:serial :fdb-streaming-mode-serial)
					      (:want-all :fdb-streaming-mode-want-all)
					      (:exact :fdb-streaming-mode-exact))
					    iteration
					    (convert-to-foreign snapshot :boolean)
					    (convert-to-foreign reverse :boolean))))
    (multiple-value-bind (kv-array more) (block-and-get-keyvalue-array future)
      (fdb-future-destroy future)
      (values kv-array more))))

(defun transaction-commit (transaction)
  "Commits a transaction. Returns true on success, signals an error on failure."
  (let ((future (fdb-transaction-commit transaction)))
    (check-error (fdb-future-block-until-ready future))
    (fdb-future-destroy future)
    t))

(defun transaction-on-error (transaction error-code)
  "Handles a transaction error. Retries if appropriate. Returns true on success."
  (let ((future (fdb-transaction-on-error transaction error-code)))
    (check-error (fdb-future-block-until-ready future))
    (fdb-future-destroy future)
    t))

(defmacro with-transaction ((transaction-var db-var) &body body)
  "Creates a new transaction, executes the body, and automatically handles commit and destroy.
   Handles errors using `transaction-on-error` for retries."
  `(let ((,transaction-var (database-create-transaction ,db-var))
	 results)
     (tagbody retry-transaction
        (handler-case (setq results (list ,@body))
          (fdb-error (c)
            (if (transaction-on-error ,transaction-var (fdb-error-code c))
                (progn (fdb-transaction-reset ,transaction-var)
		       (format t "called ~a~%~%~%" (list (fdb-error-code c) c))
                       (go retry-transaction))
                (error c)))))
     (transaction-commit ,transaction-var)
     (fdb-transaction-destroy ,transaction-var)
     results))

;;; Miscellaneous

;;; Mutation type helpers
(defun add-op (transaction key param)
  "Performs an addition atomic operation."
  (transaction-atomic-op transaction key param :fdb-mutation-type-add))

(defun bit-and-op (transaction key param)
  "Performs a bitwise AND atomic operation."
  (transaction-atomic-op transaction key param :fdb-mutation-type-bit-and))

(defun bit-or-op (transaction key param)
  "Performs a bitwise OR atomic operation."
  (transaction-atomic-op transaction key param :fdb-mutation-type-bit-or))

(defun bit-xor-op (transaction key param)
  "Performs a bitwise XOR atomic operation."
  (transaction-atomic-op transaction key param :fdb-mutation-type-bit-xor))

(defun compare-and-clear-op (transaction key param)
  "Performs a compare-and-clear atomic operation."
  (transaction-atomic-op transaction key param :fdb-mutation-type-compare-and-clear))

(defun max-op (transaction key param)
  "Performs a maximum atomic operation."
  (transaction-atomic-op transaction key param :fdb-mutation-type-max))

(defun byte-max-op (transaction key param)
  "Performs a lexicographic maximum atomic operation."
  (transaction-atomic-op transaction key param :fdb-mutation-type-byte-max))

(defun min-op (transaction key param)
  "Performs a minimum atomic operation."
  (transaction-atomic-op transaction key param :fdb-mutation-type-min))

(defun byte-min-op (transaction key param)
  "Performs a lexicographic minimum atomic operation."
  (transaction-atomic-op transaction key param :fdb-mutation-type-byte-min))

(defun set-versionstamped-key-op (transaction key param)
  "Performs a set-versionstamped-key atomic operation."
  (transaction-atomic-op transaction key param :fdb-mutation-type-set-versionstamped-key))

(defun set-versionstamped-value-op (transaction key param)
  "Performs a set-versionstamped-value atomic operation."
  (transaction-atomic-op transaction key param :fdb-mutation-type-set-versionstamped-value))

(defun transaction-get-estimated-range-size-bytes (transaction begin-key end-key)
  "Returns the estimated size in bytes of the specified key range in the given transaction"
  (with-foreign-string ((c-begin-key begin-key-len) begin-key)
    (with-foreign-string ((c-end-key end-key-len) end-key)
      (let ((future (fdb-transaction-get-estimated-range-size-bytes transaction c-begin-key begin-key-len c-end-key end-key-len)))
        (unwind-protect
             (block-and-get-int64 future)
          (fdb-future-destroy future))))))

(defun transaction-get-range-split-points (transaction begin-key end-key chunk-size)
  "Returns a list of keys that can be used to split the given range into roughly equally sized chunks"
  (with-foreign-string ((c-begin-key begin-key-len) begin-key)
    (with-foreign-string ((c-end-key end-key-len) end-key)
      (let ((future (fdb-transaction-get-range-split-points transaction c-begin-key begin-key-len c-end-key end-key-len chunk-size)))
        (unwind-protect
             (block-and-get-key-array future)
          (fdb-future-destroy future))))))

;;; High-level functions

(defun fdb-set (key value)
  "Sets a key-value pair in the database. Uses the global *db* connection."
  (with-transaction (tr *db*)
    (transaction-set tr key value)))

(defun fdb-get (key)
  "Gets the value associated with a key from the database. Uses the global *db* connection."
  (with-transaction (tr *db*)
     (transaction-get tr key)))

(defun fdb-get-range (start stop)
  "get a range from start to stop"
  (with-transaction (tr *db*)
    (transaction-get-range tr start stop)))
