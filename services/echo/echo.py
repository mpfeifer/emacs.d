import selectors
import logging
import logging.handlers
import signal
import sys
import os
import configparser
import getopt
import time
import socket

# Author     : Matthias
# Description: Python script template

class Application:

    def __init__(self):
        self.name          = 'echo'
        self.version       = '1.0'
        self.loghost_port  = logging.handlers.DEFAULT_TCP_LOGGING_PORT
        self.loghost_name  = 'localhost'
        self.server_port   = 14001
        self.server_name   = 'localhost'
        self.logdomain     = 'mp.services.echo'
        self.usage_string  = 'Usage: echo.py -h'
        self.inifile       = None
        self.inifile_name  = 'undefined'
        self.log           = self.get_logger()
        self.sel           = selectors.DefaultSelector()
        self.setup_network_logger()
        self.gather_parameter()
        signal.signal(signal.SIGINT, Application.signal_int_handler)
        
    def get_logger(self):
        log = logging.getLogger(self.logdomain)
        formatstring='%(asctime)s %(name)s %(levelname)s %(message)s'
        formatter = logging.Formatter(formatstring)
        handler = logging.StreamHandler(sys.stderr)
        handler.setFormatter(formatter)
        log.addHandler(handler)
        log.setLevel(logging.DEBUG)    
        log.propagate=1
        return log

    def gather_parameter(self):

        '''Collect parameters from inifile (first) and then from commandline.'''

        first_getopt_index=1

        if len(sys.argv)> 1 and not sys.argv[1].startswith('-'):
            self.inifile_name=sys.argv[1]
            first_parameter_index=2

        if os.path.isfile(self.inifile_name):
            self.inifile=configparser.ConfigParser()

        if self.inifile:
            self.loghost_name=inifile.get('[logging]', 'hostname', fallback=self.loghost_name)
            self.loghost_port=inifile.get('[logging]', 'port', fallback=self.loghost_port)

        try:
            opts, args = getopt.getopt(sys.argv[first_getopt_index:], 'hp:l:', 'help')
        except getop.GetoptError as err:
            print(err)
            self.usage()

        output = None
        verbose = False

        for option, arg in opts:
            if option == '-h':
                self.usage();
            elif option == '-p':
                self.loghost_port=int(argument)
            elif option == '-l':
                self.loghost_name=argument

    def usage(self):
        print(self.usage_string)
        exit(-1)

    @staticmethod
    def signal_int_handler(signal, frame):
        interrupt_msg = 'Script terminated by keyboard interrupt'
        print(interrupt_msg)
        exit(0)

    def setup_network_logger(self):
        rootLogger = logging.getLogger('')
        rootLogger.setLevel(logging.DEBUG)
        socketHandler = logging.handlers.SocketHandler(self.loghost_name,
                                                       self.loghost_port)
        rootLogger.addHandler(socketHandler)

    def accept(self, sock, mask):
        conn, addr = sock.accept()  # Should be ready
        self.log.info('New connection from ' + str(addr))
        conn.setblocking(False)
        read_wrapper = lambda x, y: self.accept(x, y)
        self.sel.register(conn, selectors.EVENT_READ, read_wrapper)

    def read(self, conn, mask):
        data = conn.recv(1000)  # Should be ready
        if data:
            log.info('echoing %d bytes to %s' % (sys.getsizeof(data), conn.getpeername()))
            conn.send(data)  # Hope it won't block
        else:
            log.info('closing connection to ' + str(conn.getpeername()))
            self.sel.unregister(conn)
            conn.close()

    def run(self):
        sock = socket.socket()
        sock.setblocking(False)
        sock.bind((self.server_name, self.server_port))
        sock.listen(10)
        accept_wrapper = lambda x, y: self.accept(x, y)
        self.sel.register(sock, selectors.EVENT_READ, accept_wrapper)

        self.log.info('echo server up on %s:%d' % (self.server_name, self.server_port))

        while True:
            events = self.sel.select()
            for key, mask in events:
                callback = key.data
                callback(key.fileobj, mask)

#
# End of application class
##

app = None

if __name__ == '__main__':
    #
    # This is where all the magic happens...
    #
    app = Application()
    app.log.info('Script is starting')
    app.run()
    app.log.info('Script is done')

#
# Done
#
# # # end of script

