package org.zaluum.nide.palette
import java.io.InputStream
import scala.Option.option2Iterable
import scala.collection.mutable.Buffer
import scala.collection.Map
import org.zaluum.nide.images.ImageMap
import org.zaluum.nide.images.ImageKey
import scala.collection.mutable.ListBuffer

object XMLParser {
  def load(file: InputStream): (List[PaletteEntry], List[(ImageKey, String)]) = {
    if (file == null) return (List(), List())
    else {
      try {
        val node = xml.XML.load(file)
        var entries = ListBuffer[PaletteEntry]()
        var imageEntries = ListBuffer[(ImageKey, String)]()
        for (entry ← node.child) {
          if (entry.label == "image")
            imageEntries ++= processImage(entry)
          if (entry.label == "entry")
            processEntry(entry).foreach { entries += _ }
        }
        (entries.toList, imageEntries.toList)
      } catch {
        case e: Exception ⇒
          println("Error parsing palette file " + file + e)
          (List(), List())
      } finally {
        file.close
      }
    }
  }
  def textOption(s: String) = s match {
    case null ⇒ None
    case ""   ⇒ None
    case o    ⇒ Some(o)
  }
  def processImage(img: xml.Node): Option[(ImageKey, String)] = {
    val name = (img \ "@class").text.trim
    val method = textOption((img \ "@method").text.trim)
    val signature = textOption((img \ "@signature").text.trim)
    val url = img.text.trim
    if (name != "" && url != "")
      Some(new ImageKey(name, method, signature), url)
    else None
  }
  def processEntry(entry: xml.Node): Option[PaletteEntry] = {
    val name = (entry \ "@name").text.trim
    val pkg = (entry \ "@pkg").text.trim
    val tpe = (entry \ "@type").text.trim
    val params = (entry \ "param").flatMap { n ⇒ (n \ "@key").headOption.map(k ⇒ (k.text -> n.text)) }
    var map = Map[String, List[String]]()
    for ((k, v) ← params) {
      if (map.contains(k)) map += (k -> (map(k) :+ v))
      else map += (k -> List(v))
    }
    if (name != "")
      Some(PaletteEntry(name, pkg, tpe, map))
    else None
  }

}