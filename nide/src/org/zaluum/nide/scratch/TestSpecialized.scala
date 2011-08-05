package org.zaluum.nide.scratch
import Numeric.Implicits._
class TestSpecialized {
  def add[A: Numeric](a: A, b: A): A = {
    a + b
  }
}