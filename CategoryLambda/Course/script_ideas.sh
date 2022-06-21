#!/usr/bin/bash


# -- Create Document from Wikipedia page
FILE="text"
WORD="Biology"
alias translate="trans -brief"

echo '---
author: Danyiel Colin
title: Mi resumen
---

## Primer tema

' > "$FILE".md

wikit "$WORD" \
	| trans -brief en:cs \
	| trans -brief cs:es \
	| fold -w60 -s >> "$FILE".md

iconv -t utf-8 -c "$FILE".md
pandoc "$FILE".md -o "$FILE".pdf --pdf-engine=xelatex
