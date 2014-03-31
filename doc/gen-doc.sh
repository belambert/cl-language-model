#!/bin/sh

todos.sh
fixme.sh

echo "Generating Lisp documentation..."
mkdir -p ./lisp

for folder in `find ../src -type d`; do
    # Remove the "../src/" prefix.
    dest_folder=./lisp/${folder:7}
    # And run Lispdoc
    lispdoc.sh $folder/ $dest_folder language-model
done

echo "Converting Lisp documentation to PDF..."
cd lisp
wkhtmltopdf --load-error-handling ignore index.html [a-hm-v]*.html */*.html */*/*.html ../kbasr-doc.pdf
cd -

if [ -f ../python ]; then
    echo "Generating Python documentation..."
    mkdir -p ./python
    cd ./python
    pydoc -w ../../python/
    cd ..
fi


