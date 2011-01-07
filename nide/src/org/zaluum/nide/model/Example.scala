package org.zaluum.nide.model
import org.zaluum.nide.zge._
object Example {
  def testModel = {
      val model = new Model;
        val a = new Box();
        a.name = "A"
        a.pos = (0, 0);
        a.size = (50, 50);
        a.className = "graystone.zaluum.TestBox";
        a.ports += new Port(a,"a")
        a.ports += new Port(a,"b")
        val ac = new Port(a,"c")
        a.ports +=ac
        model.boxes += a

        val b = new Box();
        b.name = "B"
        b.pos = (100, 100);
        b.size = (50, 50);
        b.className = "graystone.zaluum.TestBox";
        b.ports += new Port(b,"a")
        b.ports += new Port(b,"b")
        val bc = new Port(b,"c")
        b.ports += bc
        model.boxes += b
        
        val s = new Box();
        s.name = "S"
        s.pos = (100, 100);
        s.size = (50, 50);
        s.className = "graystone.zaluum.TestBox";
        val sa = new Port(s,"a")
        val sb = new Port(s,"b")
        s.ports += sa
        s.ports += sb
        s.ports += new Port(s,"c")
        model.boxes += s
        
        model.connections += new Connection(Some(ac), Some(sa))
        model.connections += new Connection(Some(bc), Some(sb))
        model.className = "graystone.zaluum.SumSum"
      model
    }
}