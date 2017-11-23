package scalan

import scala.wrappers.WrappersModule
import scalan.collection.{ColsModule, ColsOverArraysModule}

trait Library extends Scalan
  with WrappersModule
  with ColsModule
  with ColsOverArraysModule
