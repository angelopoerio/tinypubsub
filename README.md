# tinypubsub
In memory pub/sub service written in Erlang/OTP

# How to build & run
Just run: ./compile.sh;./start.sh

# Supported commands

It listens on the TCP port 4444 for the following commands:

PUB key value - publish a new key 

SUB key       - subscribe to the changes of the given key

UNSUB key     - unsubscribe from the given key


# Docker container with all the setup
  How to use:

```bash
  cd docker
  sudo docker build -t tinypubsub .
  sudo docker run -p 4444:4444 tinypubsub
```
  You should get a tinypubsub instance up and running in the container listening on the right port

# Future improvements
* Add persistance (Mnesia?)
* Distribution (it is Erlang!)

# Author & Notes
  Written by Angelo Poerio <angelo.poerio@gmail.com>

  Please note this piece of software is not production ready! Use it at your own risk!

