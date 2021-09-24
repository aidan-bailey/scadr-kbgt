# Knowledge Base Generation Tool (SKGT)

SKGT requires Java 16 to run owing to the TweetyProject dependency.

[sbt](https://www.scala-sbt.org/1.x/docs/Setup.html) is required to build the project.

To run unit tests, use

```sh
sbt test
```

To compile, from the main directory, run

```sh
sbt assembly
```

A **jar** file will be compiled to the `target/scala-2.13` directory.

This can be run using

```sh
scala skgt-assembly-*.0.jar --help
```
