# Stefan Wehr, 2006-06-14
#
# A very simple webserver with support for CGI.
#
# The document root is the current directory. CGI scripts are searched in
# ./cgi-bin.

import sys
import socket
from BaseHTTPServer import HTTPServer
from CGIHTTPServer import CGIHTTPRequestHandler

DEFAULT_PORT = 8080

def start(port):
    addr = ('', port)
    try:
        httpd = HTTPServer(addr, CGIHTTPRequestHandler)
    except socket.error, e:
        error('ERROR: %s' % e[1])
    print "Listening at port %d" % port
    print "Exit with Ctrl-C"
    httpd.serve_forever()

def error(s):
    print >> sys.stderr, s
    sys.exit(1)
    
def usage():
    error("Usage: %s [PORT]\n\nThe default port is %d" % (sys.argv[0],
                                                          DEFAULT_PORT))

if __name__ == '__main__':
    n = len(sys.argv)
    if n == 1:
        port = DEFAULT_PORT
    elif n == 2:
        if sys.argv[1] in ['--help', '-help', '-h', '-?']:
            usage()
        else:
            try:
                port = int(sys.argv[1])
            except ValueError:
                error("Invalid port: %s" % sys.argv[1])
    else:
        usage()
    try:
        start(port)
    except KeyboardInterrupt:
        print "\nExiting..."
        sys.exit(0)
