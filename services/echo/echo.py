#!/usr/bin/python3

import argparse
import logging
import logging.handlers
import selectors
import signal
import socket
import sys

# Author     : Matthias
# Description: Python script template

quit =  False

class Application:

    name               = 'Echo client'
    version            = '1.0'
    log                = None
    properties         = None
    parser             = None
    args               = None
    sel                = selectors.DefaultSelector()
    server_port        = 14001
    server_iface       = "127.0.0.1"
    logdomain          = "mp.services.echo"
    backlog            = 10
    quit               = False
 
    def __init__(self):
        signal.signal(signal.SIGINT, Application.signal_int_handler)
        parser = argparse.ArgumentParser(description="", epilog="")
        parser.add_argument("-v", "--verbose", help="Be more verbose when logging", action="store_true")
        parser.add_argument("-P", "--properties", help="A properties file for use by the application", type=str)
        parser.add_argument("-l", "--loghost", help="Name of host to receive log messages", default="127.0.0.1")
        parser.add_argument("-o", "--logport", help="Port of service to receive log messages", type=int, default=logging.handlers.DEFAULT_TCP_LOGGING_PORT)
        parser.add_argument("-d", "--logdomain", help="Domain for logging", default=self.logdomain)
        parser.add_argument("-r", "--remotelog", help="Enable remote logging with default host and port", action="store_true")
        parser.add_argument("-p", "--port", help = "Server port number", default = self.server_port, type = int)
        parser.add_argument("-b", "--backlog", help = "Size of backlog", default = self.backlog)
        parser.add_argument("-i", "--interface", help = "Interface for server", default = self.server_iface)
        self.args = parser.parse_args()
        self.parser = parser
        self.setup_logging()
        self.read_properties(self.args.properties)
        self.log.info('{} {} is starting'.format(self.name, self.version))
        
    def setup_logging(self):
        """ Setup logging so that a root logger is configured with formatter and handler
        according to configuration. Additional loggers should just propagate to the root
        logger. """
        self.log = logging.getLogger(self.args.logdomain)
        rootlogger = logging.getLogger()
        formatstring='%(asctime)s %(levelname)-15s %(name)s # %(message)s'
        formatter = logging.Formatter(fmt=formatstring, datefmt='%d.%m.%y %I:%M:%S')
        handler = None
        if self.args.remotelog:
            handler = logging.handlers.SocketHandler(self.loghost_name, self.loghost_port)
        else:
            handler = logging.StreamHandler(sys.stderr)
        handler.setFormatter(formatter)
        rootlogger.addHandler(handler)
        level = logging.INFO
        if self.args.verbose:
            level = logging.DEBUG
        self.log.setLevel(level)
        rootlogger.setLevel(level)
        self.log.propagate=1
        
    def read_properties(self, filename):
        """ Treat the file with given filename as a properties file. """
        if filename:
            properties = {}
            comment_char = "#"
            seperator = ":"
            with open(filename, "rt") as f:
                for line in f:
                    l = line.strip()
                    if l and not l.startswith(comment_char):
                        key_value = l.split(seperator)
                        key = key_value[0].strip()
                        value = seperator.join(key_value[1:]).strip()
                        properties[key] = value 
            self.properties = properties

    @staticmethod
    def signal_int_handler(signal, frame):
        global quit
        quit = True
        log = logging.getLogger("signalhandler")
        log.propagate = 1
        interrupt_msg = 'Termination requested by keyboard interrupt'
        log.info(interrupt_msg)

    def run(self):
        args =  self.args

        sock = socket.socket()
        sock.setblocking(False)
        sock.bind((args.interface, args.port))
        sock.listen(args.backlog)

        def read(conn, mask):
            data = conn.recv(1000)  # Should be ready
            if data:
                strdata =  data[0:20].decode('utf-8', 'replace')
                if (strdata == "QUIT\r\n"):
                    self.log.info( 'Termination requested by client' )
                    self.quit =  True
                else:
                    self.log.info('echoing {} bytes to client {}'.format(len(data), conn.getpeername()))
                    conn.send(data)
                    
            else:
                self.log.info('closing connection to ' + str(conn.getpeername()))
                self.sel.unregister(conn)
                conn.close()

        def accept(sock, mask):
            conn, addr = sock.accept()  # Should be ready
            self.log.info('New connection from ' + str(addr))
            conn.setblocking(False)
            self.sel.register(conn, selectors.EVENT_READ, read)

        self.sel.register(sock, selectors.EVENT_READ, accept)

        self.log.info('Script is up and ready to serv clients on {}:{}'.format(args.interface, args.port))

        global quit
        while (not self.quit) and (not quit):
            self.log.debug('Main loop goes into New iteration')
            events = self.sel.select()
            for key, mask in events:
                callback = key.data
                callback(key.fileobj, mask)

        self.sel.close()
        self.log.info('Script terminated')
            
def main():
    app = Application()
    app.run()

if __name__ == '__main__':
    main()

#
# Done
#
# # # end of script
