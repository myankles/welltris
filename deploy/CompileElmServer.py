#!/usr/bin/env python

# This is a tiny webserver which compiles the elm source code to
# javascript before serving up index.html.


from SimpleHTTPServer import SimpleHTTPRequestHandler as SimpleHandler
import SocketServer
import subprocess
import sys


class CompileElmHandler(SimpleHandler):
    def do_GET(self):
        # rebuild the elm code if asking for the main page
        if self.path in ['/deploy/', '/deploy/index.html']:
            # run the make command
            make = subprocess.Popen('Make', stdout=subprocess.PIPE)
            (outData, errData) = make.communicate()
            sys.stdout.write(outData)

            # output any errors
            if make.returncode != 0:
                # show the error
                self.send_response(200)
                self.send_header('Content-type','text/plain')
                self.end_headers()
                self.wfile.write("Error.\n\n" + outData)
                return

        # now just serve the page
        SimpleHandler.do_GET(self)


def main():
    # get the port
    try:
        port = int(sys.argv[1])
    except:
        print "Usage: %s [port]" % sys.argv[0]
        port = 8000

    # run the local server
    print "serving at port", port
    httpd = SocketServer.TCPServer(("", port), CompileElmHandler)
    httpd.serve_forever()


if __name__ == '__main__':
    main()
