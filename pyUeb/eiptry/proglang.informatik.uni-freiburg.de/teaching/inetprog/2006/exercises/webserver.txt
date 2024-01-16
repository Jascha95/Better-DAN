# Stefan Wehr, 2006-06-14
#
# A very simple webserver with support for CGI.
#
# The document root is the current directory. CGI scripts are searched in
# ./cgi-bin.

import sys
from BaseHTTPServer import HTTPServer
from CGIHTTPServer import CGIHTTPRequestHandler

DEFAULT_PORT = 8080

def start(port):
    addr = ('', port)
    httpd = HTTPServer(addr, CGIHTTPRequestHandler)
    print "Listening at port %d" % port
    httpd.serve_forever()

def error(s):
    print >> sys.stderr, s
    sys.exit(1)
    
if __name__ == '__main__':
    n = len(sys.argv)
    if n == 1:
        port = DEFAULT_PORT
    elif n == 2:
        try:
            port = int(sys.argv[1])
        except ValueError:
            error("Invalid port: %s" % sys.argv[1])
    else:
        error("Usage: %s [PORT]\n\nThe default port is %d" % (sys.argv[0],
                                                              DEFAULT_PORT))
    try:
        start(port)
    except KeyboardInterrupt:
        print "\nExiting..."
        sys.exit(0)
