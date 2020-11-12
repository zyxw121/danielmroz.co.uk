#!/bin/bash
cd $(dirname $0)
stack build
stack exec website
ssh webserver service nginx stop
scp -r public/* webserver:/var/www/danielmroz.co.uk/html
ssh webserver service nginx start
