#!/bin/bash

echo "compiling tinypubsub..."

if [ ! -d "bin" ]
then
	mkdir bin
	cp src/pub_sub_app.app bin
fi

cd bin
erlc ../src/*.erl
