package org.zaluum.nide.runtime

abstract class RunnableBox {
  def contents()
  def apply() {
    contents()
  }
}

abstract class LoopBox extends RunnableBox{
  def cond:Boolean
  override def apply() {
    while (cond) {
      contents()
    }
  }
}