package org.zaluum.nide.compiler

import scala.collection.mutable.ListBuffer
import org.junit.Assert._
import org.junit.Test
import junit.framework.TestCase
import org.junit.Before

class NameTest {
  def verify(str:String, res:String, resdim:Int) {
    val arr = Name(str).asArray
    assert (arr.isDefined)
    val (name,dim) = arr.get
    assertEquals(name.str,res)
    assertEquals(dim,resdim)
  }
  def verifyInvalid(str:String) {
    assertTrue(Name(str).asArray.isEmpty)
  }
  @Test def verifyArrays() { 
    verify("foo[]","foo",1)
    verify("foo [ ]","foo",1)
    verify("foo [] [ ]   [] ", "foo", 3)
    verifyInvalid("")
    verifyInvalid("a]")
    verifyInvalid("a[[")
    verifyInvalid("a[]]")
    verifyInvalid("a[][][]]")
    verifyInvalid("bar")
  }
}
