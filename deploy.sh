#!/bin/bash

ssh -i ~/.ssh/dig root@46.101.84.95 service nginx stop
scp -r public/* suzy:/var/www/html
ssh -i ~/.ssh/dig root@46.101.84.95 service nginx start
