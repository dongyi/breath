import socket 
import time
import sys
import simplejson
HOST = ""
PORT = 54321

send_data = simplejson.dumps({"name":"dongyi","age":26})
s = socket.socket(socket.AF_INET, socket.SOCK_STREAM)



s.connect((HOST, PORT))

while True:
    input_str = raw_input()
    if input_str == 'quit':
        s.close()
        break
    s.send(input_str)
    data = s.recv(2048)
    print "recv... ", data

s.close
