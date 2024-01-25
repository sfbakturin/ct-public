#!/bin/bash

if [ ! -d src/main/java/bakturin/lab3/antlr/ ]
then
  mkdir src/main/java/bakturin/lab3/antlr/
else
  rm src/main/java/bakturin/lab3/antlr/*
fi

antlr4 \
  src/main/resources/Py2C.g4 \
  -o src/main/java/bakturin/lab3/antlr/ \
  -package bakturin.lab3.antlr \
  -Xexact-output-dir
