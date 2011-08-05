package org.zaluum.nide.scratch
import java.io.FileInputStream
import java.nio.charset.Charset

import scala.collection.mutable.Buffer

object CheckEncoding {
  def read = {
    val in = new FileInputStream("/home/frede/devel/zaluum/example/src/org/zaluum/example/a.zaluum")
    val buff = Buffer[Byte]()
    var b = in.read()
    while (b != -1) {
      buff += b.byteValue
      b = in.read()
    }
    in.close
    buff.toArray
  }
  def main(args: Array[String]) {
    val bytes = read
      def encodeDecode(charSet: String) {
        val cd: String = new String(bytes, Charset.forName(charSet));
        val newb = cd.getBytes(Charset.forName(charSet))
        val equal = bytes.sameElements(newb);
        println("charSet " + charSet + " " + equal + " " + newb.length + " " + bytes.length)
      }
    encodeDecode("UTF-8")
    encodeDecode("US-ASCII")
    encodeDecode("ISO-8859-1")

  }
}