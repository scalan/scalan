## Versioning

[sbt-release](https://github.com/sbt/sbt-release) plugin is used. While we are in 0.x.y stage, minor incompatible changes can bump the bugfix (last) version part.

## Build

Current `scalaVersion` is set to 2.10. Projects targeted to scala 2.10 are
aggregated under `scalan`:

    > project scalan
    > compile

Project `frontend` is different, it can be built only against scala 2.11. Its
dependencies `core` and `common` are cross-compiled against scala 2.10 and
2.11. To make sbt resolve dependencies of `frontend` project correctly, we need
to cross-compile it (sbt command `+`) first.

    > project frontend
    > + compile
    > test

Important to mention that cross-compilation modifies `scalaVersion` globally to
the version of the target project. You need to `reload` sbt to reset
configuration.
