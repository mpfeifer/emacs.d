import configparser
import getopt
import logging
import logging.handlers
import os
import signal
import sys
import time

from LogServer import LogRecordStreamHandler
from LogServer import LogRecordSocketReceiver

# Author     : Matthias
# Description: Python script template

class Application:

    def __init__(self):
        self.inifile       = None
        self.inifile_name  = 'undefined'
        self.logdomain     = 'mp.demo.services.loghost'
        self.loghost_name  = 'localhost'
        self.loghost_port  = logging.handlers.DEFAULT_TCP_LOGGING_PORT
        self.name          = 'loghost'
        self.description   = 'Receive log records from remote hosts'
        self.tcpserver     = None
        self.usage_string  = 'Usage: loghost.py'
        self.version       = '1.0'

        self.gather_parameter()
        self.log     = self.get_logger()
        signal.signal(signal.SIGINT, Application.signal_int_handler)
        
    def get_logger(self):
        log = logging.getLogger()
        formatstring='%(asctime)s %(name)s %(levelname)s %(message)s'
        formatter = logging.Formatter(formatstring)
        handler = logging.StreamHandler(sys.stderr)
        handler.setFormatter(formatter)
        log.addHandler(handler)
        log.setLevel(logging.DEBUG)
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

    def run(self):
        self.tcpserver = LogRecordSocketReceiver()
        self.tcpserver.serve_until_stopped()
        
def main():
    #
    # This is where all the magic happens...
    #
    app = Application()
    app.log.info('{} {} is starting'.format(app.name, app.version))
    app.run()
    app.log.info('{} {} is done'.format(app.name, app.version))

if __name__ == '__main__':
    main()

#
# Done
#
# # # end of script




if __name__ == '__main__':
    main()
