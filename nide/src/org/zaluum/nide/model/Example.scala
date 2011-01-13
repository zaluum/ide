package org.zaluum.nide.model
import org.zaluum.nide.zge._
object Example {
  def sumsumModel = {
    val model = new Model;
    val a = Box(model,"A", "graystone.zaluum.SumBox", Point(0, 0))
    val b = Box(model,"B", "graystone.zaluum.SumBox", Point(100, 100))
    val s = Box(model,"S", "graystone.zaluum.SumBox", Point(200, 200))
    Connection(model,a,"c",s,"a")
    Connection(model,b,"c",s,"b")
    PortDecl(model,"out")
    model.className = "graystone.zaluum.SumSum"
    model
  }
  def printModel = {
    val model = new Model;
    val a = Box(model,"A", "graystone.zaluum.ConstBox", Point(0,0))
    val b = Box(model,"B", "graystone.zaluum.ScalaPrintBox",Point(100,100))
    Connection(model,a,"o",b,"a")
    model.className = "graystone.zaluum.PrintResult"
    model
  }
}