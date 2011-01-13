package org.zaluum.nide.compiler
import graystone.zaluum.TestTest
object TestExecution {
  def executeClass (s:String) {
    val o = Class.forName(s).newInstance
    val app = o.asInstanceOf[{def apply():Unit}]
    app.apply()
    
  }
  def main(args: Array[String]): Unit = {
    executeClass("graystone.zaluum.PrintResult");
    executeClass("graystone.zaluum.SumSum");
    
    //val o = new TestTest();
    //o.asInstanceOf[{def apply():Unit}].apply()
    /*val b = new graystone.zaluum.TestBox()
    val s = new graystone.zaluum.SumSum() 
    s.apply()*/
  }

}