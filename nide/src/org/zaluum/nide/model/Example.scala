package org.zaluum.nide.model
import org.zaluum.nide.zge._
object Example {
  def sumsumModel = {
    val model = new Model;
    val a = Box("A", "graystone.zaluum.SumBox", (0, 0), (50, 50))
    Port(a, "a")
    Port(a, "b")
    val ac = Port(a, "c")
    model.boxes += a

    val b = Box("B", "graystone.zaluum.SumBox", (100, 100), (50, 50))
    Port(b, "a")
    Port(b, "b")
    val bc = Port(b, "c")
    model.boxes += b

    val s = Box("S", "graystone.zaluum.SumBox", (200, 200), (50, 50))
    val sa = Port(s, "a")
    val sb = Port(s, "b")
    Port(s, "c")
    model.boxes += s

    model.connections += new Connection(Some(ac), Some(sa))
    model.connections += new Connection(Some(bc), Some(sb))
    model.className = "graystone.zaluum.SumSum"
    model
  }
  def printModel = {
    val model = new Model;
    val a = Box("A", "graystone.zaluum.ConstBox", (0,0), (50,50))
    val ao = Port(a, "o")
    model.boxes += a

    val b = Box("B", "graystone.zaluum.ScalaPrintBox",(100,100),(50,50) )
    val ba = Port(b, "a")
    model.boxes += b

    model.connections += new Connection(Some(ao), Some(ba))
    model.className = "graystone.zaluum.PrintResult"
    model
  }
}