package graystone.zaluum

import graystone.zaluum.interpreter.SimpleFunc
import graystone.zaluum.annotations._




object TestCompiler {
  def main (args : Array[String]) {
    val sum1 = new BoxM("sum1","graystone.zaluum.TestBox")
    val sum1a = sum1.in("a","Double")
    val sum1b = sum1.in("b","Double")
    val sum1c = sum1.out("c","Double")
    val sum2 = new BoxM("sum2","graystone.zaluum.TestBox")
    val sum2a = sum2.in("a","Double")
    val sum2b = sum2.in("b","Double")
    val sum2c = sum2.out("c","Double")
    val run = new ConnectionBoxM("con","simple")
    val runa= run.in("a","Double","1.0")
    val runb= run.in("b","Double","1.0")
    val runc= run.out("c","Double","1.0")
    run.boxes += sum1
    run.boxes += sum2
    run.connections ++= List(
        (runa -> sum1a),
        (runa -> sum1b), 
        (runb -> sum2b),
        (sum1c -> sum2a),
        (sum2c -> runc)
        )
    /*val jvm = Interpreter2.compile(run)
    jvm.act()
    println (jvm.outputs.head())*/ 
  }
}