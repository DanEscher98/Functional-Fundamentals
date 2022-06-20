#!/usr/bin/bash

for i in $(iconv -l | tr -d ',' | tr -d '/'); do
	formated="$(iconv -t $i ctext.md)"
	if [ $? -eq 0 ]; then
		echo "ERROR"
	else
		if [[ "$formated" == *"illegal"* ]]; then
			echo "ERROR"
		elif [[ "$formated" == *"warning"* ]]; then
			echo "ERROR"
		else
			echo $i
			echo $formated
			printf "\n\n"
		fi
	fi
done
