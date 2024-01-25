#!/bin/bash

if [ "$#" -ne 3 ]
then
  echo "Usage: <script> <expression> <dot filename> <image filename>"
  exit 1
fi

JAVA_PATH_MAIN_DIR="./src/main/java"
JAVA_PATH_GRAPHICS_JAVA="${JAVA_PATH_MAIN_DIR}/bakturin/lab2/Graphics.java"
JAVA_PATH_GRAPHICS="bakturin.lab2.Graphics"

EXPRESSION="${1}"
FILENAME="${2}"
SVG="${3}"

javac -cp "${JAVA_PATH_MAIN_DIR}" "${JAVA_PATH_GRAPHICS_JAVA}"
java -cp "${JAVA_PATH_MAIN_DIR}" "${JAVA_PATH_GRAPHICS}" "${EXPRESSION}" "${FILENAME}"

dot -Tsvg "${FILENAME}" -o "${SVG}"
open "${SVG}"
