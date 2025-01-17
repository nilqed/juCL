#!/bin/sh

# Install kernel.json
jupyter kernelspec install ./ --name='jucl' --user
jupyter kernelspec list

# Search for Python user site 
PYUSRSITE=$(python3 -m site --user-site)

# Copy kernel and server
mkdir -p -v $PYUSRSITE/jucl
cp -v ./jucl.py $PYUSRSITE/jucl
cp -v ./jucl.lisp $PYUSRSITE/jucl

echo "Test: $ jupyter notebook ### then  New -> juCL"
