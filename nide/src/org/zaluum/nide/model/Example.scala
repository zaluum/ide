package org.zaluum.nide.model
import org.zaluum.nide.zge._
object Example {
  def encapsulatePrint = {
    val model = new Model
    model.className = "graystone.zaluum.EncapsulatePrint"
    val a = Box(model, "A", "graystone.zaluum.ScalaPrintBox", Point(0, 0))
    val in = PortDecl(model, "in", true, Point(0, 0), Point(0, 0),"D")
    Connection(model, in, a, "a")
    model
  }
  def sumsumModel = {
    val model = new Model;
    model.className = "graystone.zaluum.SumSum"
    val a = Box(model, "A", "graystone.zaluum.SumBox", Point(0, 0))
    val b = Box(model, "B", "graystone.zaluum.SumBox", Point(100, 100))
    val s = Box(model, "S", "graystone.zaluum.SumBox", Point(200, 200))
    Connection(model, a, "c", s, "a")
    Connection(model, b, "c", s, "b")
    val out = PortDecl(model, "out", false, Point(0, 0), Point(0, 0),"D")
    Connection(model, s, "c", out)
    model
  }
  def printModel = {
    val model = new Model;
    model.className = "graystone.zaluum.PrintResult"
    val a = Box(model, "A", "graystone.zaluum.ConstBox", Point(0, 0))
    val b = Box(model, "B", "graystone.zaluum.ScalaPrintBox", Point(100, 100))
    Connection(model, a, "o", b, "a")
    model
  }
}