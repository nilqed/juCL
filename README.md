# juCL
A (almost) fool-proof Jupyter kernel for Common Lisp

:warning: in progress :construction:

Instead of implementing the kernel in CL itself, we use 
IPython’s kernel machinery[^1] and 
__Hunchentoot__[^2] (the awesome Common Lisp web server) for the
communication.

## Installation
There a three files necessary

1. `jucl.py` -- the wrapper kernel (python3)
2. `jucl.lisp` -- loads Hunchentoot (POPEN in `jucl.py`)
3. `kernel.json` -- the kernel spec

To install them, clone this repo and either run `./ìnstall.sh` or perform the commands manually:

    # Install kernel.json
    jupyter kernelspec install ./ --name='jucl' --user
    jupyter kernelspec list

    # Search for Python user site 
    PYUSRSITE=$(python3 -m site --user-site)

    # Copy kernel and server
    mkdir -p -v $PYUSRSITE/jucl
    cp -v ./jucl.py $PYUSRSITE/jucl
    cp -v ./jucl.lisp $PYUSRSITE/jucl

The prerequisites are of course:

* a Lisp (only __SBCL__ tested at the moment)
* Python 3: `jupyter` and the `requests` library (pip).

## Start
After installation try 

   $ cd /tmp
   $ jupyter notebook
   
choose `juCL` 

![select-kernel](./docs/jucl1.jpg?raw=true)

Try some CL commands

![notebook](./docs/jucl2.jpg?raw=true)

Notice the terminal, where `SBCL` is running. One may ignore it, however,
it accepts cl commands as well (sharing common env!).

![terminal](./docs/jucl3.jpg`raw=true)

Last but not least, there is the jupyter console showing the log.

![jupyter-log](./docs/jucl4.jpg?raw=true)


TODO: coming more ...

📅 17-JAN-2025 


[^1]:https://jupyter-client.readthedocs.io/en/stable/wrapperkernels.html
[^2]:https://edicl.github.io/hunchentoot/
