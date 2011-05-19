package org.zaluum.nide.eclipse.integration.model

object JDTInternalUtils {
  def aToString(aa:Array[Array[Char]]) : String =  aa.map{new String(_)}.mkString(".")
  def stringToA(str:String) : Array[Array[Char]]=  str.split('.').map{_.toCharArray}
}