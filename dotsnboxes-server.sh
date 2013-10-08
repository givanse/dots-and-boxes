#!/bin/bash

SERVER_NAME="server@host.com"

erl -name $SERVER_NAME -setcookie dnbcookie -run serverDnb start

#EOF
