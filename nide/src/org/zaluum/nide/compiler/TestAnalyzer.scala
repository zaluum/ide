package org.zaluum.nide.compiler

import java.io.FileInputStream
import java.io.File
import org.zaluum.nide.protobuf.BoxFileProtos
import org.zaluum.nide.protobuf.ZaluumProtobuf
object TestAnalyzer {
  def main(args:Array[String]) {
    val f = new FileInputStream("../example/src/org/zaluum/example/testInner.zaluum")
    
    val bcd = ZaluumProtobuf.BoxClass.parseDelimitedFrom(f)
    val tree = Parser.parse(bcd)
    val reporter = new Reporter()
    new Analyzer(reporter, tree, null)
  }
} 