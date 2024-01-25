#!/bin/bash

JAVA_PATH_MAIN_DIR="./src/main/java"
JAVA_PATH_MAIN_JAVA="${JAVA_PATH_MAIN_DIR}/bakturin/lab2/Main.java"
JAVA_PATH_MAIN="bakturin.lab2.Main"

javac -cp "${JAVA_PATH_MAIN_DIR}" "${JAVA_PATH_MAIN_JAVA}"
java -cp "${JAVA_PATH_MAIN_DIR}" "${JAVA_PATH_MAIN}" 
