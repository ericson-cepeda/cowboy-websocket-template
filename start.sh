#!/bin/sh
erl -name template@127.0.0.1 -noshell -pa ebin deps/*/ebin -s template

 
