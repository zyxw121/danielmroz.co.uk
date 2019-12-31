#!/bin/bash
cd $(dirname $0)
stack build
stack exec website
ssh -i ~/.ssh/dig root@46.101.84.95 service nginx stop
scp -r public/* suzy:/var/www/html
ssh -i ~/.ssh/dig root@46.101.84.95 service nginx start
