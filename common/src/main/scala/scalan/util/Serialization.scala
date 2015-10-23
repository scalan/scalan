package scalan.util

import java.io.{ByteArrayInputStream, ObjectOutputStream, ByteArrayOutputStream}
import java.util.zip.{GZIPInputStream, GZIPOutputStream}
import javax.xml.bind.DatatypeConverter

import scala.reflect.{classTag, ClassTag}

object Serialization {
  def save(obj: Any): String = {
    val bos = new ByteArrayOutputStream()
    try {
      val gzip = new GZIPOutputStream(bos)
      try {
        val objOut = new ObjectOutputStream(gzip)
        try {
          objOut.writeObject(obj)
          objOut.close()
          val str = DatatypeConverter.printBase64Binary(bos.toByteArray)
          str
        } finally objOut.close()
      } finally gzip.close()
    } finally bos.close()
  }

  def load[A: ClassTag](obj: String): A = {
    val bytes = DatatypeConverter.parseBase64Binary(obj)
    val bis = new ByteArrayInputStream(bytes)
    try {
      val gzip = new GZIPInputStream(bis)
      try {
        // required instead of ObjectInputStream to work correctly from SBT, see
        // www.scala-sbt.org/0.13/docs/Running-Project-Code.html
        val objIn = new ThreadContextClassLoaderObjectInputStream(gzip)
        try {
          val obj = objIn.readObject()
          classTag[A].runtimeClass.cast(obj).asInstanceOf[A]
        } finally objIn.close()
      } finally gzip.close()
    } finally bis.close()
  }

}
