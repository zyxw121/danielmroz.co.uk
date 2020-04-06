#!/bin/bash
cd $(dirname $0)
stack build
stack exec website
ssh kitty service nginx stop
scp -r public/* kitty:/var/www/danielmroz.co.uk/html
ssh kitty service nginx start
