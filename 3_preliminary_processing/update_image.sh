#!/bin/bash

docker ps -a | awk 'NR>1 {print $1}' | xargs sudo docker rm
docker image ls | awk 'NR>1 {print $3}' | xargs sudo docker image rm
docker compose up
