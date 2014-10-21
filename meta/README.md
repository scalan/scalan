# Scalan-meta

Scalan-meta is a boilerplate code generator for [Scalan](https://github.com/scalan/scalan)-based DSLs.

## Usage

Edit `src/main/scala/scalan/meta/BoilerplateTool.scala` file and add your own config or edit one of the existing ones. List the files for which code should be generated in `entityFiles` field (paths are relative to `srcPath`). Run `BoilerplateTool`.

Setting config from command-line arguments is planned, contact us if you need it.