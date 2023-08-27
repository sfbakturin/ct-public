#!/bin/bash

JAVA_ADVANCED_DIR="../../java-advanced-2023"
JAVA_SOLUTIONS_DIR="../java-solutions"
SOLUTIONS_MODULE_DIR="info/kgeorgiy/ja/bakturin"
ARTIFACTS_DIR="${JAVA_ADVANCED_DIR}/artifacts"
ARTIFACT_IMPLEMENTOR_DIR="${ARTIFACTS_DIR}/info.kgeorgiy.java.advanced.implementor.jar"

IMPLEMENTOR_DIR=${JAVA_SOLUTIONS_DIR}/${SOLUTIONS_MODULE_DIR}/implementor/*.java
IMPLER_DIR=${JAVA_ADVANCED_DIR}/modules/info.kgeorgiy.java.advanced.implementor/info/kgeorgiy/java/advanced/implementor/*Impler*.java

IMPLEMENTOR_CLASS_DIR=${SOLUTIONS_MODULE_DIR}/implementor/*.class
IMPLER_CLASS_DIR=info/kgeorgiy/java/advanced/implementor/*.class

JAR_NAME="Implementor.jar"

javac -cp "${ARTIFACT_IMPLEMENTOR_DIR}" \
      -d "." \
       ${IMPLEMENTOR_DIR} ${IMPLER_DIR}

jar -cfm "${JAR_NAME}" \
    MANIFEST.MF \
    ${IMPLEMENTOR_CLASS_DIR} ${IMPLER_CLASS_DIR}
