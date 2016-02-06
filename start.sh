#!/bin/bash

erl -pa bin/ -noshell -eval 'application:start(pub_sub_app)'
