import logging
import logging.handlers
import signal
import sys
import os
import configparser
import getopt
import socket
from datetime import datetime

# Author     : Matthias
# Description: Python script template

#
# "global" variables
#

loghost_port=logging.handlers.DEFAULT_TCP_LOGGING_PORT
loghost_name="localhost"
dateserver_port=14002
dateserver_host="localhost"
logdomain='mp.services.dat'

def usage():
    print("Usage: date.py -h -p -l -H")
    print("   -h     print this help")
    print("   -p     remote logging connection port number")
    print("   -l     remote logging connection hostname")
    print("   -P     date server port number")
    print("   -H     date server bind address")
    exit(-1)

def signal_int_handler(signla, frame):
    msg="Script terminated by keyboard interrupt"
    print(msg)
    logging.info(msg)
    exit(0)

def setup_network_logger():
    rootLogger = logging.getLogger('')
    rootLogger.setLevel(logging.DEBUG)
    socketHandler = logging.handlers.SocketHandler(loghost_name,
                                                   loghost_port)
    rootLogger.addHandler(socketHandler)

def get_logger():
    log = logging.getLogger(logdomain)

    formatstring='%(asctime)s %(name)s %(levelname)s %(message)s'
    formatter = logging.Formatter(formatstring)

    handler = logging.StreamHandler(sys.stderr)
    handler.setFormatter(formatter)

    log.addHandler(handler)
    log.setLevel(logging.DEBUG)    
    log.propagate=1

    return log


def initialize():

    """Collect parameter from inifile (first), then from commandline."""

    inifile=None
    inifile_name="undefined"
    first_getopt_index=1

    if len(sys.argv)> 1 and not sys.argv[1].startswith('-'):
        inifile_name=sys.argv[1]
        first_parameter_index=2

    if os.path.isfile(inifile_name):
        inifile=configparser.ConfigParser()

    if inifile:
        loghost_name=inifile.get('[logging]', 'hostname', fallback=loghost_name)
        loghost_port=inifile.get('[logging]', 'port', fallback=loghost_port)
        loghost_port=inifile.get('[date]', 'port', fallback=port)

    try:
        opts, args = getopt.getopt(sys.argv[first_getopt_index:], 'hp:l:P:H:', "help")
    except getop.GetoptError as err:
        print(err)
        usage()

    output = None
    verbose = False

    for option, arg in opts:
        if option == "-h":
            usage();
        elif option == "-p":
            loghost_port=int(argument)
        elif option == "-l":
            loghost_name=argument
        elif option == "-P":
            dateserver_port=int(argument)
        elif option == "-H":
            dateserver_host=(argument)            

    signal.signal(signal.SIGINT, signal_int_handler)
    setup_network_logger()

def main():

    """Start date server. Server will bind and listen for incoming connection."""

    logging.info('Python date.py date server starting')

    s = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
    s.bind((dateserver_host, dateserver_port))
    s.listen(1)

    while True:
        conn, addr = s.accept()
        logging.info('New connection from %s:%d' % (addr[0], addr[1]))
        timestr = str(datetime.today())
        conn.sendall(timestr.encode())
        conn.close()

    logging.info("date server date.py is done")

initialize()

#
# Main
#

if __name__ == "__main__":
    main()

#
# Done
#
# # # end of script

