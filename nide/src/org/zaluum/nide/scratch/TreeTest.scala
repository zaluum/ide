package org.zaluum.nide.scratch
import scala.collection.immutable.TreeSet
import scala.annotation.tailrec

object TreeTest {
  case class PaletteEntry(pkgName: String, bla: String)
  case class Pkg(val name: String, val child: List[Pkg], val entries: List[PaletteEntry])
  /**
   * children must be sorted by packageName
   */
  def doPkg(meAndChildren: Stream[PaletteEntry]): (Option[Pkg], Stream[PaletteEntry]) = {
    meAndChildren match {
      case me #:: children ⇒
        var s = children
        var child = List[Pkg]()
        var entries = List(me)
        while (!s.isEmpty && s.head.pkgName.startsWith(me.pkgName)) {
          if (s.head.pkgName == me.pkgName) {
            entries ::= s.head
            s = s.tail
          } else {
            val (pkg, next) = doPkg(s)
            child :::= pkg.toList
            s = next
          }
        }
        (Some(Pkg(me.pkgName, child, entries)), s)
      case Stream.Empty ⇒ (None, Stream.Empty)
    }
  }

  def main(args: Array[String]) {
    val s = List(
      PaletteEntry("a", "1"),
      PaletteEntry("aab", "2"),
      PaletteEntry("aab", "4"),
      PaletteEntry("aab", "5"),
      PaletteEntry("acd", "3"),
      PaletteEntry("acd", "7"),
      PaletteEntry("afd", "4")).sortBy(_.pkgName)
    /* val (pkg, ignore) = doPkg(s.toStream)
    println(pkg)*/
  }
}