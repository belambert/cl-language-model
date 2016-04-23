;;;; Ben Lambert (ben@benjaminlambert.com)

(in-package :language-model)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;; Global vocab variables ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defstruct vocab
  "A new struct to replace the six global variables below."
  (words #())
  (word-table (make-hash-table :test 'equal))
  (concepts #())
  (concept-table (make-hash-table :test 'equal))
  (combined #())
  (combined-table (make-hash-table :test 'equal))
  (word-min 200)
  (word-max nil)
  (concept-min 1000)
  (concept-max nil)
  (shared-min 0)
  (shared-max nil))

(defvar *global-vocab* (make-vocab)
  "The global active vocabulary.  This hold 3 vocabs:
    - a word vocabulary
    - a concept vocabulary
    - and a shared vocab, containing both.
   The shared vocab *should* be the union of the word and concept vocabs, if anything at all.")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;; Un-register common words from the KB ;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar *pronoun-unregister-list* (list "a" "the" "i" "me" "myself" "you" "yourself" "he" "she" "him" "her" "himself" "herself" "we" "us" "ourselves" "they" "them" "themselves" "it" "itself" "what" "that" "where" "is" "be")
  "A hard-coded list of strings to un-register from the KB")

(defun unregister-common-words-from-kb (vocab-file n)
  "Unregister the top n words in the given vocab file from the KB.
   This does *not* unregister the word 'thing' b/c we need that string handle on it..."
  (let* ((vocab (file->line-list vocab-file))
	 (unregister-list (truncate-list vocab n)))
    (setf unregister-list (concatenate 'list unregister-list *pronoun-unregister-list*))
    ;; don't unregister "thing"... we need it.
    (setf unregister-list (remove "thing" unregister-list :test 'string-equal))
    (dolist (word unregister-list)
      (dolist (e (mapcar 'first (cl-user::lookup-definitions word)))
	(cl-user::unregister-definition word e)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun get-global-word-id (word)
  "Get the numerical id for a word."
  (setf word (make-string-upper-case word))
  (gethash word (vocab-word-table *global-vocab*)))

(defun get-global-word-from-id (id)
  "Get the word corresponding to a numerical word id."
  (elt (vocab-words *global-vocab*) id))

(defun get-global-concept-id (concept)
  "Get the numerical ID for a concept."
  (gethash concept (vocab-concept-table *global-vocab*)))

(defun get-global-concept-from-id (id)
  "Get the concept corresponding to a numerical concept ID."
  (elt (vocab-concepts *global-vocab*) id))

(defun get-global-shared-id (word-or-concept)
  "Get the ID of a word or a concept from the shared active vocabulary."
  (when (stringp word-or-concept)
    (setf word-or-concept (make-string-upper-case word-or-concept)))
  (gethash word-or-concept (vocab-combined-table *global-vocab*)))

(defun get-global-shared-from-id (id)
  "Get the concept corresponding to a numerical concept ID."
  (elt (vocab-concepts (vocab-combined *global-vocab*)) id))

(defun get-global-word-vocab-id (word)
  "Get the numerical id for a word."
  (let ((id (get-global-word-id word)))
    (if (and id
	     (or (not (vocab-word-min *global-vocab*)) (>= id (vocab-word-min *global-vocab*)))
	     (or (not (vocab-word-max *global-vocab*)) (<= id (vocab-word-max *global-vocab*))))
	id
	nil)))

(defun get-global-concept-vocab-id (concept)
  "Get the numerical id for a word."
  (let ((id (get-global-concept-id concept)))
    (if (and id
	     (or (not (vocab-concept-min *global-vocab*)) (>= id (vocab-concept-min *global-vocab*)))
	     (or (not (vocab-concept-max *global-vocab*)) (<= id (vocab-concept-max *global-vocab*))))
	id
	nil)))

(defun get-global-shared-vocab-id (concept)
  "Get the numerical id for a word."
  (let ((id (get-global-shared-id concept)))
    (if (and id
	     (or (not (vocab-shared-min *global-vocab*)) (>= id (vocab-shared-min *global-vocab*)))
	     (or (not (vocab-shared-max *global-vocab*)) (<= id (vocab-shared-max *global-vocab*))))
	id
	nil)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;; Active vocabulary setup ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun get-vocab-list-and-table (filename)
  "Given a frequency file filename, read and return two values: a list of the vocab items, and a table mapping from vocab to id's."
  (let ((vocab (coerce (mapcar 'first (read-frequency-file filename)) 'vector)))
    (map-into vocab 'lookup-element-special vocab)
    (values vocab (freq-list->id-table vocab))))

(defun setup-vocab (&key wvocab cvocab)
  "Read-in a word and/or concept vocabulary and put the vocab into the global vocab variable.
   Also creates a combined vocab."
  ;; Read the word vocab
  (when (and wvocab (not (string-equal wvocab "NIL"))) ;; This string-equal to NIL is so we can put "NIL" in a shell script...
    (multiple-value-bind (list table)
	(get-vocab-list-and-table wvocab)
      (setf (vocab-words *global-vocab*) list)
      (setf (vocab-word-table *global-vocab*) table)))
  ;; Read the concept vocab
  (when (and cvocab (not (string-equal cvocab "NIL")))
    (multiple-value-bind (list table)
	(get-vocab-list-and-table cvocab)
      (setf (vocab-concepts *global-vocab*) list)
      (setf (vocab-concept-table *global-vocab*) table)))
  ;; Create a combined vocab.
  (setf (vocab-combined *global-vocab*) (concatenate 'vector (vocab-words *global-vocab*) (vocab-concepts *global-vocab*)))
  (setf (vocab-combined-table *global-vocab*) (freq-list->id-table (vocab-combined *global-vocab*)))
  ;; Print out some a summary of what we loaded
  (format t "Setup vocab finished.~%")
  (format t "Word vocabulary size:    ~7:D~%" (length (vocab-words *global-vocab*)))
  (format t "Concept vocabulary size: ~7:D~%" (length (vocab-concepts *global-vocab*)))
  (format t "Shared vocabulary size:  ~7:D~%" (length (vocab-combined *global-vocab*)))
  (force-output t))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;; Misc...  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun count-vocab-in-kb ()
  "Count the number of word in the current active vocabulary that are in the loaded KB."
  (let ((counter 0))
    (dolist (word (vocab-words *global-vocab*))
      (when (cl-user::lookup-definitions word)
	(incf counter)))
    counter))

(defun vocab-list->vocab-table (vocab-list)
  "Given a 'vocab list' (i.e. '((w1 40) (w2 39 )..)) convert it into 
   a hash table."
  (freq-list->boolean-hash-table vocab-list))

(defun freq-list->boolean-hash-table (list)
  "Take a frequency list and turn it into a boolean hashtable.  Useful for setting up a vocab table."
  (let ((items (mapcar 'first list))
	(table (make-hash-table :test 'equal)))
    (dolist (item items)
      (setf (gethash item table) t))
    table))

(defun freq-list->id-table (list)
  "Take a frequency list and turn it into a boolean hashtable.  Useful for setting up a vocab table."
  (let ((table (make-hash-table :test 'equal))
	(counter 0))
    (setf list (coerce list 'vector))
    (loop for e across list do
	 (when (stringp e)
	   (setf e (string-upcase e)))
	 (setf (gethash e table) counter)
	 (incf counter))
    table))

(defun prune-freq-count-list (freq-list &key (prune-head 200) (min-count 2) (top-n nil) (minimum-string-length 4) (prune-non-alpha t))
  "Given a frequency list, prune it as specified."
  (let* ((full-vocab-size (length freq-list))
	 (start-num prune-head)
	 (end-num (if top-n
		      top-n
		      (or (position-if (lambda (x) (< (elt x 1) min-count)) freq-list)
			  (length freq-list)))))
    (setf freq-list (subseq freq-list (min start-num (length freq-list)) (min end-num (length freq-list))))
    (setf freq-list (remove-if (lambda (x) (and (stringp x) (< (length x) minimum-string-length))) freq-list :key 'first))
    (when prune-non-alpha
      (setf freq-list (remove-if (lambda (x) (notevery 'alpha-char-p x)) freq-list :key 'first)))
    (format t "Pruned freq count list to size ~A.  Indicies of original vocab [~a,~A]; full vocab size: ~A~%" (length freq-list) start-num end-num full-vocab-size)
    freq-list))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;; Counting words ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun text->wfreq (transcript-filename wfreq-filename)
  "The is the main function for turning a text file into a wfreq file."
  (let ((freq-list (count-file-tokens transcript-filename)))
    (print-frequency-file freq-list wfreq-filename)))

(defun text->cfreq (transcript-filename freq-filename word->concept-function wvocab-filename &key origin-file)
  "Read in a text file and compute concept counts.  Write those concept counts to specified folder."
  (multiple-value-bind (freq-list origin-table)
      (count-concept-frequencies-in-text transcript-filename word->concept-function
					 :wvocab wvocab-filename
					 :save-origins origin-file)
    (print-frequency-file freq-list freq-filename)
    (when origin-file
      (print-origins-file origin-file freq-list origin-table))
    (length freq-list)))

(defun read-frequency-file (filename)
  "Read a frequency file.  This is one line per word with the word, some space and then the count."
  (let ((freq '()))
    (blambert-util::do-lines (line filename)
      (setf line (read-from-string line))
      (push line freq))
    (sort freq '> :key 'second)))

(defun print-frequency-file (freq-list filename)
  "Given a frequency list, print it to a frequency file."
  (format t "Printing freq file to ~A.~%" filename)
  (with-open-file (outfile filename :direction :output :if-exists :supersede :if-does-not-exist :create)
    (dolist (pair freq-list)
      (format outfile "(~{~S~^ ~})~%" pair)))
  (length freq-list))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;; Frequencies to vocabularies ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun freq-file->vocab-file (freq-file vocab-file &key (prune-head 200) (min-count 2) (top-n nil))
  "Convert a frequency file to a vocab file, by performing the specified pruning."
  (let ((freq-list (read-frequency-file freq-file)))
    (setf freq-list (prune-freq-count-list freq-list :prune-head prune-head :top-n top-n :min-count min-count))
    (print-frequency-file freq-list vocab-file)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;; Counting words and concepts in text. ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun word-is-reasonable? (word)
  "Check if the length of a word is less than 50 characters, and contains
   fewer than four non-alphanumeric characters."
  (not (or (equal word "")
	   (> (length word) 50)
	   (> (count-if-not 'alphanumericp word) 4))))

(defun count-tokens (word-list)
  "Count the number of tokens for each word type in the list.  Return as a list of pairs <word, freq>."
  (let ((count-table (make-hash-table :test 'equalp))
	(word-count-pairs '()))
    (dolist (word word-list)
      (unless (string-equal word "")
	(setf word (string-upcase word))
	(incf (gethash word count-table 0))))
    (loop for k being the hash-keys in count-table using (hash-value v)
       do (push (list k v) word-count-pairs))
    (setf word-count-pairs (sort word-count-pairs (lambda (x y) (> (second x) (second y)))))
    word-count-pairs))

(defun count-concepts (word-list get-concepts-function vocab-table &key count-table origin-table)
  "Given a words list, and a function for turning those words into concepts,
   compute counts for each concepts that is encountered."
  (let ((tokens-in-vocab 0)
	(tokens-with-concepts 0))
    (dolist (word word-list)
      (when (or (not vocab-table) (gethash word vocab-table))
	(incf tokens-in-vocab)
	(let ((concept-list (funcall get-concepts-function word)))
	  (when concept-list (incf tokens-with-concepts))
	  (dolist (concept concept-list)
	    (incf (gethash concept count-table 0))
	    (when origin-table
	      (push word (gethash concept origin-table)))))))
    (values tokens-in-vocab tokens-with-concepts)))

(defun count-file-tokens (filename)
  "Count the words in a file... like above, but from a file.  Designed for use with the Gigaword data."
  (let ((count-table (make-hash-table :test 'equalp))
	(word-count-pairs '())
	(token-count 0))
    (do-lines-fast-limited-memory (line filename :characters-per-batch 100000000)
      (let ((words (cl-ppcre:split "\\\s+" line)))
	(dolist (word words)
	  (when (word-is-reasonable? word)
	    (incf token-count)
	    (incf (gethash word count-table 0))))))
    (loop for k being the hash-keys in count-table using (hash-value v)
       do (push (list k v) word-count-pairs))
    (format t "Counted ~A word types from ~A tokens.~%" (hash-table-count count-table) token-count)
    (setf word-count-pairs (sort word-count-pairs (lambda (x y) (> (second x) (second y)))))
    word-count-pairs))
 
(defun count-concept-frequencies-in-text (filename get-concepts-function &key wvocab (save-origins nil))
  "Compute frequencies of concepts in a transcript file."
  (multiple-value-bind (vocab-list vocab-table)
      (get-vocab-list-and-table wvocab)  ;; use this word vocab!
    (declare (ignore vocab-list))
    (let* ((count-table (make-hash-table :test 'equal))
	   (origin-table (when save-origins (make-hash-table :test 'equal)))
	   (tokens-in-vocab 0)
	   (tokens-in-kb 0))
      (format t "Counting concepts using vocabulary of size ~A~%" (hash-table-count vocab-table))
      ;; Iterate over all the lines in the file, updating the hash-tables as we go
      (do-lines-fast-limited-memory (line filename :characters-per-batch 100000000)
	(let ((words (mapcar 'string-upcase (cl-ppcre:split "\\\s+" line))))
	  (multiple-value-bind (this-tokens-in-vocab this-tokens-in-kb)
	      (count-concepts words get-concepts-function vocab-table
			      :count-table count-table
			      :origin-table origin-table)
	    (incf tokens-in-vocab this-tokens-in-vocab)
	    (incf tokens-in-kb this-tokens-in-kb))))
      (format t "Counted ~A tokens in the vocabulary.  ~A of those tokens are in the KB.~%" tokens-in-vocab tokens-in-kb)
      (when save-origins (organize-origin-table origin-table))
      (let ((count-list (sort (blambert-util::hash-table->list count-table) #'> :key 'second)))
	(values count-list origin-table)))))

(defun count-fof (word-list)
  "Compute a frequency of frequencies list from the given word list."
  (let ((token-count-list (count-tokens word-list))
	(fof-table (make-hash-table)))
    (dolist (pair token-count-list)
      (incf (gethash (second pair) fof-table 0)))
    (sort (blambert-util::hash-table->list fof-table) '> :key 'second)))

(defun organize-origin-table (origin-table)
  "Call list->count-list on each entry in the origin-table."
  (loop for key being the hash-keys of origin-table
     for origin-list = (gethash key origin-table) do
       (setf origin-list (list->count-list origin-list))
       (setf (gethash key origin-table) origin-list))
  origin-table)

(defun print-origins-file (filename freq-list origin-table)
  "Print from which words we got the counts for the concepts, using the origins hash table."
  (with-open-file (file filename :direction :output :if-exists :supersede :if-does-not-exist :create)
    (loop for freq-pair in freq-list
       for word = (first freq-pair)
       for count = (second freq-pair)
       for origin-list = (gethash word origin-table) do
	 (format file "~A ~A tokens, from ~A word types -- ~{~{~A ~}~}~%" word count (length origin-list) origin-list))))
