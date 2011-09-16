package org.zaluum.nide.scratch
import org.zaluum.nide.utils.MethodBindingUtils

object SignatureTest {
  def main(args: Array[String]): Unit = {
    val test = "org.zaluum.Bla#meme(IIVV[[Lsdf/asf/ASdf;)[I"
    println(test)
    MethodBindingUtils.staticMethod(test)
    println(MethodBindingUtils.signatureToTypes("(II[[[Zorg/zaluum/CACA;)V"))
  }
}