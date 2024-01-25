plugins {
    kotlin("jvm") version "1.9.10"
    java
    application
}

group = "ru.itmo.mpp"

repositories {
    mavenCentral()
}

java {
    toolchain {
        languageVersion.set(JavaLanguageVersion.of(17))
    }
}

sourceSets.main {
    java.srcDir("src")
}

sourceSets.test {
    java.srcDir("test")
}

application {
    mainClass.set("VerifyMonotonicClockKt")
}

tasks["build"].dependsOn("run")