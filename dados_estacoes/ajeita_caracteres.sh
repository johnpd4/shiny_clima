#!/bin/bash

for dir in */; do
	for file in $dir*; do
		echo $file
		iconv "$file" -c | sponge "$file"
	done
done
