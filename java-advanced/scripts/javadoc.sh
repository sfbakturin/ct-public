#!/bin/bash

LINK_JDK_17="https://docs.oracle.com/en/java/javase/17/docs/api/"
JAVA_ADVANCED_DIR="../../java-advanced-2023"
JAVA_SOLUTIONS_DIR="../java-solutions/info/kgeorgiy/ja/bakturin"
LIB_DIR="${JAVA_ADVANCED_DIR}/lib"
ARTIFACTS_DIR="${JAVA_ADVANCED_DIR}/artifacts"
OUTPUT_DIR="../javadoc/"

IMPLEMENTOR_DIR=${JAVA_SOLUTIONS_DIR}/implementor/*.java
IMPLER_DIR=${JAVA_ADVANCED_DIR}/modules/info.kgeorgiy.java.advanced.implementor/info/kgeorgiy/java/advanced/implementor/*Impler*.java

javadoc -d "${OUTPUT_DIR}" \
        -link "${LINK_JDK_17}" \
        -private \
        -author \
        -version \
        -cp "${LIB_DIR}" \
        -cp "${ARTIFACTS_DIR}" \
        ${IMPLEMENTOR_DIR} ${IMPLER_DIR}
