#!/bin/bash

SERVER_NAME="server@host.com"

echo "Connecting to server: "$SERVER_NAME

erl -name client@host.com -setcookie dnbcookie -run dnb start $SERVER_NAME 
#erl -name client@host.com -setcookie dnbcookie

#EOF
