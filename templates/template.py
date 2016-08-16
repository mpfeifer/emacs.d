#!/usr/bin/python3

import logging
import logging.handlers
import signal
import sys
import os
import configparser
import getopt
import time

# Author     : Matthias
# Description: Python script template

class Application:
    name               = 'template'
    version            = '0.1'
    def __init__(self):
        self.loghost_port  = logging.handlers.DEFAULT_TCP_LOGGING_PORT
        self.loghost_name  = 'localhost'
        self.logdomain     = 'mp.py.template'
        self.usage_string  = 'Usage: this-script.py [inifile] -h -p <numeric> -l <hostname> -r\r\n\r\n'
        self.usage_string+=' [inifile]  if inifile is set it is read before commandline switches\r\n'
        self.usage_string+='    -h      print usage string\r\n'
        self.usage_string+='    -l      set remote logging host\r\n'
        self.usage_string+='    -p      set remote logging port\r\n'
        self.usage_string+='    -r      enable remote logging\r\n'
        self.usage_string+='\r\nIf remote logging is enabled in inifile it cannot be disabled via commandline.\r\n'
        self.usage_string+='{} version {}'.format(Application.name, Application.version)
        self.inifile       = None
        self.inifile_name  = 'undefined'
        self.log           = self.get_logger()
        self.remote_logger_enabled = 0
        self.gather_parameter()
        signal.signal(signal.SIGINT, Application.signal_int_handler)
        self.log_configuration()

    def log_configuration(self):
        self.log.debug("%28s = %s", "Application.name", Application.name)
        self.log.debug("%28s = %s", "version", Application.version)
        self.log.debug("%28s = %s", "inifile_name", self.inifile_name)
        self.log.debug("%28s = %s", "log", self.log)
        self.log.debug("%28s = %s", "remote_logger_enabled", self.remote_logger_enabled)
        self.log.debug("%28s = %s", "loghost_port", self.loghost_port)
        self.log.debug("%28s = %s", "loghost_name", self.loghost_name)

    def get_logger(self):
        log = logging.getLogger(self.logdomain)
        formatstring='%(asctime)s %(levelname)-15s %(name)s # %(message)s'
        formatter = logging.Formatter(fmt=formatstring)
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
            self.log.info('will read inifile ' + self.inifile_name)

        if os.path.isfile(self.inifile_name):
            self.inifile=configparser.ConfigParser()

        if self.inifile:
            self.log.info('inifile ' + self.inifile_name + "successfully opened")
            self.loghost_name=inifile.get('[logging]', 'hostname', fallback=self.loghost_name)
            self.loghost_port=inifile.get('[logging]', 'port', fallback=self.loghost_port)
            if (inifile.get('[logging]', 'network_logging', fallback=0)):
                self.setup_network_logger()
        try:
            opts, args = getopt.getopt(sys.argv[first_getopt_index:], 'hp:l:r', 'help')
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
            elif option == '-r':
                if not self.remote_logger_enabled:
                    self.setup_network_logger()

    def usage(self):
        print(self.usage_string)
        exit(-1)

    @staticmethod
    def signal_int_handler(signal, frame):
        interrupt_msg = '\r\n\r\n{} {} terminated by keyboard interrupt'.format(Application.name, Application.version)
        print(interrupt_msg)
        exit(0)

    def setup_network_logger(self):
        rootLogger = logging.getLogger('')
        rootLogger.setLevel(logging.DEBUG)
        socketHandler = logging.handlers.SocketHandler(self.loghost_name,
                                                       self.loghost_port)
        rootLogger.addHandler(socketHandler)
        self.network_logging_enabled=1

    def run(self):
        time.sleep(10)

def main():
    #
    # This is where all the magic happens...
    #
    app = Application()
    app.log.info('{} {} will be instantiated'.format(app.name, app.version))
    app.run()
    app.log.info('{} {} is done'.format(app.name, app.version))

if __name__ == '__main__':
    main()

#
# Done
#
# # # end of script
