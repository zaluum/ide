package org.zaluum.nide.runtime

trait RunnableBox {
  def apply()
}

abstract class Loop extends RunnableBox{
  def cond:Boolean
  def apply() {
    while (cond) {
      oneLoop()
    }
  }
  def oneLoop()
}