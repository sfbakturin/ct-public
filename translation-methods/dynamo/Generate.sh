#!/bin/bash

if [ ! -d src/main/java/bakturin/lab4/antlr/ ]
then
  mkdir src/main/java/bakturin/lab4/antlr/
else
  rm src/main/java/bakturin/lab4/antlr/*
fi

antlr4 \
  src/main/resources/Grammar.g4 \
  -o src/main/java/bakturin/lab4/antlr/ \
  -package bakturin.lab4.antlr \
  -visitor \
  -no-listener \
  -Xexact-output-dir
