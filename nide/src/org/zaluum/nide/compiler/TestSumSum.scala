package org.zaluum.nide.compiler
import graystone.zaluum.TestTest
object TestSumSum {

  def main(args: Array[String]): Unit = {
    val o = Class.forName("graystone.zaluum.PrintResult").newInstance
    val app = o.asInstanceOf[{def apply():Unit}]
    app.apply()
    //val o = new TestTest();
    //o.asInstanceOf[{def apply():Unit}].apply()
    /*val b = new graystone.zaluum.TestBox()
    val s = new graystone.zaluum.SumSum() 
    s.apply()*/
  }

}