============
Sphinx-L ASR decoder
============



ASR decoder.

Uses acoustic models trained by Sphinx.

Can be run in batch mode on SPhinx feature extracted files.

The 'standalone' component, operates on raw WAV files and does the feature extraction internally.


Command line usage
------------------

Run from the command line args similar to Sphinx3 decode:
scripts/sphinxlisp_decode.sh


API usage
---------

(defun ngram-model-dmp-read (filename &key debug)
  "The main function for reading binary DMP language models."

To load an ARPA LM, create a new N-gram model instance with the filename in the constructor.

...

Notes
-----

 * ...




