(in-package :fdb)

(define-foreign-library libfdb
  (:unix (:or "libfdb_c.so" "libfdb_c.so.2" "libfdb_c.so.7.3" "libfdb_c.so.7.3.57"))
  (t (:default "libfdb_c")))

(use-foreign-library libfdb)

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

;;; Basic Types

(defctype fdb-bool-t :int) ;; boolean 0 or 1
(defctype fdb-error-t :int) ;; boolean 0 or 1

;;; API Versioning

(defcfun ("fdb_get_error" fdb-get-error) :string
  (code fdb-error-t))

(defcfun ("fdb_error_predicate" fdb-error-predicate) fdb-bool-t
  (predicate-test :int)
  (code fdb-error-t))

(defcfun ("fdb_select_api_version_impl" fdb-select-api-version-impl) fdb-error-t
  "Selects the API version (implementation). For internal use, prefer select-api-version"
  (runtime-version :int)
  (header-version :int))

(defun select-api-version (version)
  "Selects the version of the FoundationDB API to use.
Must be called before any other functions."
  (check-error (fdb-select-api-version-impl version *api-version*)))

(defcfun ("fdb_get_max_api_version" fdb-get-max-api-version) :int
  "Returns the maximum API version supported by the client library.")

;;; Options (from fdb_c_options.h)

(defcenum fdb-network-option
  (:fdb-net-option-local-address 10)
  (:fdb-net-option-cluster-file 20)
  (:fdb-net-option-trace-enable 30)
  (:fdb-net-option-trace-roll-size 31)
  (:fdb-net-option-trace-max-logs-size 32)
  (:fdb-net-option-trace-log-group 33)
  (:fdb-net-option-trace-format 34)
  (:fdb-net-option-trace-clock-source 35)
  (:fdb-net-option-trace-file-identifier 36)
  (:fdb-net-option-trace-partial-file-suffix 39)
  (:fdb-net-option-knob 40)
  (:fdb-net-option-tls-plugin 41)
  (:fdb-net-option-tls-cert-bytes 42)
  (:fdb-net-option-tls-cert-path 43)
  (:fdb-net-option-tls-key-bytes 45)
  (:fdb-net-option-tls-key-path 46)
  (:fdb-net-option-tls-verify-peers 47)
  (:fdb-net-option-buggify-enable 48)
  (:fdb-net-option-buggify-disable 49)
  (:fdb-net-option-buggify-section-activated-probability 50)
  (:fdb-net-option-buggify-section-fired-probability 51)
  (:fdb-net-option-tls-ca-bytes 52)
  (:fdb-net-option-tls-ca-path 53)
  (:fdb-net-option-tls-password 54)
  (:fdb-net-option-disable-multi-version-client-api 60)
  (:fdb-net-option-callbacks-on-external-threads 61)
  (:fdb-net-option-external-client-library 62)
  (:fdb-net-option-external-client-directory 63)
  (:fdb-net-option-disable-local-client 64)
  (:fdb-net-option-client-threads-per-version 65)
  (:fdb-net-option-disable-client-statistics-logging 70)
  (:fdb-net-option-enable-slow-task-profiling 71)
  (:fdb-net-option-enable-run-loop-profiling 72)
  (:fdb-net-option-client-buggify-enable 80)
  (:fdb-net-option-client-buggify-disable 81)
  (:fdb-net-option-client-buggify-section-activated-probability 82)
  (:fdb-net-option-client-buggify-section-fired-probability 83))

(defcenum fdb-database-option
  (fdb-db-option-location-cache-size 10)
  (fdb-db-option-max-watches 20)
  (fdb-db-option-machine-id 21)
  (fdb-db-option-datacenter-id 22)
  (fdb-db-option-snapshot-ryw-enable 26)
  (fdb-db-option-snapshot-ryw-disable 27)
  (fdb-db-option-transaction-logging-max-field-length 405)
  (fdb-db-option-transaction-timeout 500)
  (fdb-db-option-transaction-retry-limit 501)
  (fdb-db-option-transaction-max-retry-delay 502)
  (fdb-db-option-transaction-size-limit 503)
  (fdb-db-option-transaction-causal-read-risky 504)
  (fdb-db-option-transaction-include-port-in-address 505)
  (fdb-db-option-transaction-bypass-unreadable 700))

(defcenum fdb-tr-options
  (:fdb-tr-option-causal-write-risky 10)
  (:fdb-tr-option-causal-read-risky 20)
  (:fdb-tr-option-causal-read-disable 21)
  (:fdb-tr-option-include-port-in-address 23)
  (:fdb-tr-option-next-write-no-write-conflict-range 30)
  (:fdb-tr-option-read-your-writes-disable 51)
  (:fdb-tr-option-read-ahead-disable 52)
  (:fdb-tr-option-durability-datacenter 110)
  (:fdb-tr-option-durability-risky 120)
  (:fdb-tr-option-durability-dev-null-is-web-scale 130)
  (:fdb-tr-option-priority-system-immediate 200)
  (:fdb-tr-option-priority-batch 201)
  (:fdb-tr-option-initialize-new-database 300)
  (:fdb-tr-option-access-system-keys 301)
  (:fdb-tr-option-read-system-keys 302)
  (:fdb-tr-option-debug-retry-logging 401)
  (:fdb-tr-option-transaction-logging-enable 402)
  (:fdb-tr-option-debug-transaction-identifier 403)
  (:fdb-tr-option-log-transaction 404)
  (:fdb-tr-option-transaction-logging-max-field-length 405)
  (:fdb-tr-option-server-request-tracing 406)
  (:fdb-tr-option-timeout 500)
  (:fdb-tr-option-retry-limit 501)
  (:fdb-tr-option-max-retry-delay 502)
  (:fdb-tr-option-size-limit 503)
  (:fdb-tr-option-snapshot-ryw-enable 600)
  (:fdb-tr-option-snapshot-ryw-disable 601)
  (:fdb-tr-option-lock-aware 700)
  (:fdb-tr-option-used-during-commit-protection-disable 701)
  (:fdb-tr-option-read-lock-aware 702)
  (:fdb-tr-option-use-provisional-proxies 711)
  (:fdb-tr-option-report-conflicting-keys 712)
  (:fdb-tr-option-special-key-space-relaxed 713)
  (:fdb-tr-option-tag 800)
  (:fdb-tr-option-auto-throttle-tag 801)
  (:fdb-tr-option-bypass-unreadable 1100))

(defcenum fdb-streaming-mode
  (:fdb-streaming-mode-want-all -2)
  (:fdb-streaming-mode-iterator -1)
  (:fdb-streaming-mode-exact 0)
  (:fdb-streaming-mode-small 1)
  (:fdb-streaming-mode-medium 2)
  (:fdb-streaming-mode-large 3)
  (:fdb-streaming-mode-serial 4))

(defcenum fdb-mutation-type
  (:fdb-mutation-type-add 2)
  (:fdb-mutation-type-and 6)
  (:fdb-mutation-type-bit-and 6)
  (:fdb-mutation-type-or 7)
  (:fdb-mutation-type-bit-or 7)
  (:fdb-mutation-type-xor 8)
  (:fdb-mutation-type-bit-xor 8)
  (:fdb-mutation-type-append-if-fits 9)
  (:fdb-mutation-type-max 12)
  (:fdb-mutation-type-min 13)
  (:fdb-mutation-type-set-versionstamped-key 14)
  (:fdb-mutation-type-set-versionstamped-value 15)
  (:fdb-mutation-type-byte-min 16)
  (:fdb-mutation-type-byte-max 17)
  (:fdb-mutation-type-compare-and-clear 20))

(defcenum fdb-conflict-range-type
  (:fdb-conflict-range-type-read 0)
  (:fdb-conflict-range-type-write 1))

(defcenum fdb-error-predicate
  (:fdb-error-predicate-retryable 50000)
  (:fdb-error-predicate-maybe-committed 50001)
  (:fdb-error-predicate-retryable-not-committed 50002))

;;; Network Setup and Teardown

(defcfun ("fdb_network_set_option" fdb-network-set-option) fdb-error-t
  "Called to set network options. If the given option is documented as taking a parameter, you must also pass a pointer to the parameter value and the parameter value’s length. If the option is documented as taking an Int parameter, value must point to a signed 64-bit integer (little-endian), and value_length must be 8. This memory only needs to be valid until fdb_network_set_option() returns."
  (option fdb-network-option)
  (value :pointer)
  (value-length :int))

(defcfun "fdb_setup_network" fdb-error-t
  "Must be called after fdb_select_api_version() (and zero or more calls to fdb_network_set_option()) and before any other function in this API. fdb_setup_network() can only be called once.")

(defun setup-network ()
  "Sets up the FDB network. Call after select-api-version."
  (check-error (fdb-setup-network)))

(defcfun "fdb_add_network_thread_completion_hook" fdb-error-t
  "Must be called after fdb_setup_network() and prior to fdb_run_network() if called at all. This will register the given callback to run at the completion of the network thread. If there are multiple network threads running (which might occur if one is running multiple versions of the client, for example), then the callback is invoked once on each thread. When the supplied function is called, the supplied parameter is passed to it."
  (hook :pointer)
  (hook-parameter :pointer))

(defcfun "fdb_run_network" fdb-error-t
  "Must be called after fdb_setup_network() before any asynchronous functions in this API can be expected to complete. Unless your program is entirely event-driven based on results of asynchronous functions in this API and has no event loop of its own, you will want to invoke this function on an auxiliary thread (which it is your responsibility to create).

This function will not return until fdb_stop_network() is called by you or a serious error occurs. It is not possible to run more than one network thread, and the network thread cannot be restarted once it has been stopped. This means that once fdb_run_network has been called, it is not legal to call it again for the lifetime of the running program.")

(defcfun ("fdb_stop_network" fdb-stop-network) fdb-error-t
  "Stops the FoundationDB network thread.")

;;; Futures

(defctype fdb-future :pointer)
(defctype fdb-cluster :pointer)
(defctype fdb-database :pointer)
(defctype fdb-transaction :pointer)

(defcfun ("fdb_future_cancel" fdb-future-cancel) :void
  "Cancels an FDBFuture object."
  (future fdb-future))

(defcfun ("fdb_future_destroy" fdb-future-destroy) :void
  "Destroys an FDBFuture object. Must be called exactly once for each future."
  (future fdb-future))

(defcfun ("fdb_future_block_until_ready" fdb-future-block-until-ready) fdb-error-t
  "Blocks until the given future is ready."
  (future fdb-future))

(defcfun ("fdb_future_is_ready" fdb-future-is-ready) fdb-bool-t
  "Returns non-zero if the future is ready."
  (future fdb-future))

(defcfun ("fdb_future_set_callback" fdb-future-set-callback) fdb-error-t
  "Sets a callback function to be invoked when the future is ready."
  (future fdb-future)
  (callback :pointer)
  (callback-parameter :pointer))

(defcfun ("fdb_future_release_memory" fdb-future-release-memory) :void
  "Releases memory associated with a future's result. For internal use."
  (future fdb-future))

(defcfun ("fdb_future_get_error" fdb-future-get-error) fdb-error-t
  "Returns the error code of a ready future, or 0 if it completed successfully."
  (future fdb-future))

(defcfun ("fdb_future_get_int64" fdb-future-get-int64) fdb-error-t
  "Extracts an int64 value from a future."
  (future fdb-future)
  (out :pointer))

(defcfun ("fdb_future_get_double" fdb-future-get-double) fdb-error-t
  "Extracts a double value from a future."
  (future fdb-future)
  (out :pointer))

(defcfun ("fdb_future_get_key" fdb-future-get-key) fdb-error-t
  "Extracts a key (byte string) from a future."
  (future fdb-future)
  (out-key :pointer)
  (out-key-length :pointer))

(defcfun ("fdb_future_get_value" fdb-future-get-value) fdb-error-t
  "Extracts a value (byte string) from a future, indicating if the key was present."
  (future fdb-future)
  (out-present :pointer)
  (out-value :pointer)
  (out-value-length :pointer))

(defcfun ("fdb_future_get_string_array" fdb-future-get-string-array) fdb-error-t
  "Extracts an array of strings from a future."
  (future fdb-future)
  (out-strings :pointer)
  (out-count :pointer))

(defcfun ("fdb_future_get_keyvalue_array" fdb-future-get-keyvalue-array) fdb-error-t
  "Extracts an array of key-value pairs from a future."
  (future fdb-future)
  (out-kv :pointer)
  (out-count :pointer)
  (out-more :pointer))

(defcfun ("fdb_future_get_key_array" fdb-future-get-key-array) fdb-error-t
  "Extracts an array of keys from a future"
  (f fdb-future)
  (out-key-array :pointer)
  (out-count :pointer))

(defcstruct fdb-key-value
  (:key :pointer)
  (:key-length :int)
  (:value :pointer)
  (:value-length :int))

;;; Database

(defcfun ("fdb_create_database" fdb-create-database) fdb-error-t
  "Creates a new database connection. For internal use, prefer connect-db."
  (cluster-file-path :string)
  (out-database :pointer))


(defcfun ("fdb_database_destroy" fdb-database-destroy) :void
  "Destroys a database connection. For internal use, `db is managed automatically."
  (database fdb-database))

(defcfun ("fdb_database_set_option" fdb-database-set-option) fdb-error-t
  "Sets a database option. For internal use."
  (database fdb-database)
  (option fdb-database-option)
  (value :pointer)
  (value-length :int))

(defcfun ("fdb_database_create_transaction" fdb-database-create-transaction) fdb-error-t
  "Creates a new transaction on the given database."
  (database fdb-database)
  (out-transaction :pointer))

(defcfun "fdb_database_reboot_worker" fdb-future
  "Reboot the specified process in the database."
  (database fdb-database)
  (address :pointer)
  (address-length :int)
  (check fdb-bool-t)
  (duration :int))

;;; Transactions

(defcfun ("fdb_transaction_destroy" fdb-transaction-destroy) :void
  "Destroys a transaction. Internal use, prefer with-transaction."
  (transaction fdb-transaction))

(defcfun ("fdb_transaction_set_option" fdb-transaction-set-option) fdb-error-t
  "Sets a transaction option."
  (transaction fdb-transaction)
  (option fdb-tr-options)
  (value :pointer)
  (value-length :int))

(defcfun ("fdb_transaction_set_read_version" fdb-transaction-set-read-version) :void
  "Sets the snapshot read version for a transaction."
  (transaction fdb-transaction)
  (version :int64))

(defcfun ("fdb_transaction_get_read_version" fdb-transaction-get-read-version) fdb-future
  "Gets the snapshot read version for a transaction."
  (transaction fdb-transaction))

(defcfun ("fdb_transaction_get" fdb-transaction-get) fdb-future
  "Reads a value from the database within a transaction."
  (transaction fdb-transaction)
  (key-name :pointer)
  (key-name-length :int)
  (snapshot fdb-bool-t))

(defcfun ("fdb_transaction_get_key" fdb-transaction-get-key) fdb-future
  "Resolves a key selector to a key within a transaction."
  (transaction fdb-transaction)
  (key-name :pointer)
  (key-name-length :int)
  (or-equal fdb-bool-t)
  (offset :int)
  (snapshot fdb-bool-t))

(defcfun ("fdb_transaction_get_addresses_for_key" fdb-transaction-get-addresses-for-key) fdb-future
  "Gets the addresses of storage servers responsible for a key."
  (transaction fdb-transaction)
  (key-name :pointer)
  (key-name-length :int))

(defcfun ("fdb_transaction_get_range" fdb-transaction-get-range) fdb-future
  "Reads a range of key-value pairs from the database within a transaction."
  (transaction fdb-transaction)
  (begin-key-name :pointer)
  (begin-key-name-length :int)
  (begin-or-equal fdb-bool-t)
  (begin-offset :int)
  (end-key-name :pointer)
  (end-key-name-length :int)
  (end-or-equal fdb-bool-t)
  (end-offset :int)
  (limit :int)
  (target-bytes :int)
  (mode fdb-streaming-mode)
  (iteration :int)
  (snapshot fdb-bool-t)
  (reverse fdb-bool-t))

(defcfun ("fdb_transaction_set" fdb-transaction-set) :void
    "Sets a key-value pair in the database within a transaction."
  (transaction fdb-transaction)
  (key-name :pointer)
  (key-name-length :int)
  (value :pointer)
  (value-length :int))

(defcfun ("fdb_transaction_clear" fdb-transaction-clear) :void
  "Clears a key from the database within a transaction."
  (transaction fdb-transaction)
  (key-name :pointer)
  (key-name-length :int))

(defcfun ("fdb_transaction_clear_range" fdb-transaction-clear-range) :void
  "Clears a range of keys from the database within a transaction."
  (transaction fdb-transaction)
  (begin-key-name :pointer)
  (begin-key-name-length :int)
  (end-key-name :pointer)
  (end-key-name-length :int))

(defcfun ("fdb_transaction_atomic_op" fdb-transaction-atomic-op) :void
  "Performs an atomic operation on a key within a transaction."
  (transaction fdb-transaction)
  (key-name :pointer)
  (key-name-length :int)
  (param :pointer)
  (param-length :int)
  (operation-type fdb-mutation-type))

(defcfun ("fdb_transaction_commit" fdb-transaction-commit) fdb-future
  "Commits a transaction."
  (transaction fdb-transaction))

(defcfun ("fdb_transaction_get_committed_version" fdb-transaction-get-committed-version) fdb-error-t
  "Gets the committed version of a transaction."
  (transaction fdb-transaction)
  (out-version :pointer))

(defcfun ("fdb_transaction_get_approximate_size" fdb-transaction-get-approximate-size) fdb-future
  "Returns an FDBFuture which will be set to the approximate transaction size so far."
  (transaction fdb-transaction))

(defcfun ("fdb_transaction_get_versionstamp" fdb-transaction-get-versionstamp) fdb-future
  "Gets the versionstamp used by any versionstamp operations in a transaction."
  (transaction fdb-transaction))

(defcfun ("fdb_transaction_watch" fdb-transaction-watch) fdb-future
  "Creates a watch on a key within a transaction."
  (transaction fdb-transaction)
  (key-name :pointer)
  (key-name-length :int))

(defcfun ("fdb_transaction_on_error" fdb-transaction-on-error) fdb-future
  "Handles a transaction error, potentially retrying the transaction."
  (transaction fdb-transaction)
  (error fdb-error-t))

(defcfun ("fdb_transaction_reset" fdb-transaction-reset) :void
  "Resets a transaction to its initial state."
  (transaction fdb-transaction))

(defcfun ("fdb_transaction_cancel" fdb-transaction-cancel) :void
  "Cancels a transaction."
  (transaction fdb-transaction))

(defcfun ("fdb_transaction_add_conflict_range" fdb-transaction-add-conflict-range) fdb-error-t
  "Adds a conflict range to a transaction."
  (transaction fdb-transaction)
  (begin-key-name :pointer)
  (begin-key-name-length :int)
  (end-key-name :pointer)
  (end-key-name-length :int)
  (type fdb-conflict-range-type))

(defcfun ("fdb_transaction_get_estimated_range_size_bytes" fdb-transaction-get-estimated-range-size-bytes) fdb-future
  "Returns an FDBFuture which will be set to the estimated size of the key range."
  (tr fdb-transaction)
  (begin-key-name :pointer)
  (begin-key-name-length :int)
  (end-key-name :pointer)
  (end-key-name-length :int))

(defcfun ("fdb_transaction_get_range_split_points" fdb-transaction-get-range-split-points) fdb-future
  "Returns a list of keys that can be used to split the given range into (roughly) equally sized chunks based on chunk_size."
  (tr fdb-transaction)
  (begin-key-name :pointer)
  (begin-key-name-length :int)
  (end-key-name :pointer)
  (end-key-name-length :int)
  (chunk-size :int64))

(defcstruct %fdb-keyvalue
  "Represents a key-value pair in FDB."
  (key :pointer)
  (key-length :int)
  (value :pointer)
  (value-length :int))
