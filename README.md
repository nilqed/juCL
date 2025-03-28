# juCL
A (almost) fool-proof Jupyter kernel for Common Lisp in very few lines of lisp and py.

:warning: in progress :construction: 
Don't use this for production!

There is an **excellent** kernel at https://github.com/yitzchak/common-lisp-jupyter

Instead of implementing the kernel in CL itself, we use 
IPython’s kernel machinery[^1] and 
__Hunchentoot__[^2] (the awesome Common Lisp web server) for the
communication.

## Installation
There are three files necessary

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

* a CL + QuickLisp (only __SBCL__ tested at the moment)
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

![terminal](./docs/jucl3.jpg?raw=true)

Last but not least, there is the jupyter console showing the log.

![jupyter-log](./docs/jucl4.jpg?raw=true)


### Error handling

![jupyter-log](./docs/errhan.jpg?raw=true)

### Quitting

![jupyter-log](./docs/quit.jpg?raw=true)


## SBCL Pipes (example)
The example notebook `docs/redpsl_pipe.ipynb` shows an example of howto
connect terminal applications by using `sb-ext:run-program` :

    (defun makepipe (app &optional args)
        (let ((proc (sb-ext:run-program app args
                     :input :stream
                     :output :stream
                     :wait nil
                     :search t)))
        (when proc
             (make-two-way-stream (sb-ext:process-output proc)
                  (sb-ext:process-input proc)))))

    ...
    

![jucl-redpsl-pipe.png](./docs/jucl-redpsl-pipe.png?raw=true)


## Weyl/SBCL Example
Notebook file and exported *.pdf (LaTeX) and *.lisp.

  * [nb-weyl-tests1](./docs/weyl-tests1.ipynb)
  * [pdf-weyl-tests1](./docs/weyl-tests1.pdf)
  * [lisp-weyl-tests1](./docs/weyl-tests1.lisp)


## STANDARD-OUPUT
Note that `juCL` displays only returned `values`, while printing to
\*standdard-output\* appears on the accompanied terminal only. If
you want to have this output in the notebook as well, then you may
use for example

    (with-output-to-string (*standard-output*) 
        (describe 'string))
     
     
where otherwise `(describe ...)` and so on return `NIL` to `juCL`.


![with-output-to-string](./docs/w-out-to-screen.png?raw=true)


## LaTeX output
A first attempt to display **LaTeX** output. 
Whenever a return value (string) starts with the `$`-character, then
the data is interpreted as a latex string. 

![latex-output](./docs/latex-output.png)


### TODO:

* coloring
* code completion
* inspection
* other formats (html,mathml, ...)




📅 17-JAN-2025 


[^1]:https://jupyter-client.readthedocs.io/en/stable/wrapperkernels.html
[^2]:https://edicl.github.io/hunchentoot/
