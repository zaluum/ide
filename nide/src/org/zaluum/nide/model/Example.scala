package org.zaluum.nide.model
import org.zaluum.nide.zge._
object Example {
  def sumsumModel = {
    val model = new Model;
    val a = Box("A", "graystone.zaluum.SumBox", Point(0, 0))
    model.boxes += a

    val b = Box("B", "graystone.zaluum.SumBox", Point(100, 100))
    model.boxes += b

    val s = Box("S", "graystone.zaluum.SumBox", Point(200, 200))
    model.boxes += s

    model.connections += new Connection(Some(PortRef(a,"c")), Some(PortRef(s,"a")))
    model.connections += new Connection(Some(PortRef(b,"c")), Some(PortRef(s,"b")))
    model.className = "graystone.zaluum.SumSum"
    model
  }
  def printModel = {
    val model = new Model;
    val a = Box("A", "graystone.zaluum.ConstBox", Point(0,0))
    model.boxes += a

    val b = Box("B", "graystone.zaluum.ScalaPrintBox",Point(100,100))
    model.boxes += b

    model.connections += new Connection(Some(PortRef(a,"o")), Some(PortRef(b,"a")))
    model.className = "graystone.zaluum.PrintResult"
    model
  }
}