# -*- coding: utf-8 -*-



from ipykernel.kernelbase import Kernel
from subprocess import Popen
import requests
import os



# Find a free port 
def get_free_port():
        import socket
        s = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
        s.bind(("",0))
        s.listen(1)
        port = s.getsockname()[1]
        s.close()
        return port

# Get new free port
htport=str(get_free_port())

# PID of CL+HT
pid=None 


class httpCL():
    """ HTTP-CL communication """
    def __init__(self, url = 'http://localhost:{0}/eval'.format(htport)):
        # Store parameters
        self.url = url
        self.output = None
        
        
    def put(self, code):
        # POST request
        payload = {'code': code}
        r = requests.post(self.url, data=payload)
        self.output = r.text
        return(r)
        


class CL(Kernel):
    """ The kernel class """
    implementation = 'cl'
    implementation_version = '1.0'
    language = 'common-lisp'
    language_version = '0.1'
    language_info = {'name': 'common-lisp',
                     'mimetype': 'text/plain',
                     'file_extension': '.lisp'}
    banner = "Common-Lisp Jupyter Kernel (Hunchentoot :)"
    
    def __init__(self, **kwargs):
        Kernel.__init__(self, **kwargs)
        self.server = httpCL()


    def do_execute(self, code, silent, store_history=True, 
      user_expressions=None, allow_stdin=False):
        
        # send code to hunchentoot and get response
        r = self.server.put(code)
       
        if not silent:
            if r.ok:
              stdout = self.server.output
              stream_content = {'name': 'stdout', 'text': stdout}
              self.send_response(self.iopub_socket, 'stream', stream_content)
            else:
              stdout = "FAILED: have a look at the  juCL terminal\n"
              stderr = {'name': 'stderr', 'text': stdout}
              self.send_response(self.iopub_socket, 'stream', stderr)

        return {'status': 'ok',
                'execution_count': self.execution_count,
                'payload': [],
                'user_expressions': {}}

if __name__ == '__main__':
    
    from ipykernel.kernelapp import IPKernelApp
    
    path = os.path.dirname(os.path.abspath(__file__))
    
    clserve = '{0}/jucl.lisp'.format(path)
    start = '(cl-serve :port {0})'.format(htport)
    terminal = ['gnome-terminal', '--title=juCL', '--'] 
    
    pid = Popen(terminal+['sbcl','--load',clserve,'--eval',start])
    
    IPKernelApp.launch_instance(kernel_class=CL)
