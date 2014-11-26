## Versioning

[sbt-release](https://github.com/sbt/sbt-release) plugin is used. While we are in 0.x.y stage, minor incompatible changes can bump the bugfix (last) version part.

## Build

Current `scalaVersion` is set to 2.10. Projects targeted to scala 2.10 are build as always:

    > reload
    > project scalan
    > compile

Project `frontend` is different, it can be built only against scala 2.11.
In order to do so, it's dependencies `core` and `common` are cross-compiled against scala 2.10 and 2.11.
To build the `frontend` project we need first cross-compile `core` and then compile `frontend` itself.

Cross compile `core` project

    > project core
    > + compile

Cross-compilation sets `scalaVersion` to 2.10 globally, and before building `frontend` we need to reload sbt shell first

    > reload
    > project frontend
    > + core/compile
    > compile
