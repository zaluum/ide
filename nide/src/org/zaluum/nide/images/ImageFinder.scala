package org.zaluum.nide.images
import java.net.URL
import org.eclipse.jdt.internal.core.JavaProject
import org.zaluum.nide.eclipse.ZaluumProject
import org.zaluum.nide.utils.Utils._

case class ImageKey(className: String, method: Option[String], selector: Option[String]) {
  def toFileNoExtension = className.replace('.', '/') + method.map("-" + _).getOrElse("") + selector.map("-" + _).getOrElse("")
  def toPng = toFileNoExtension + ".png"
  def toGif = toFileNoExtension + ".gif"
  def moreGeneral =
    if (selector != None) Some(copy(selector = None))
    else if (method != None) Some(copy(method = None))
    else None
}

class ImageMap(val zProject: ZaluumProject) {
  var map = Map[ImageKey, String]()
  def add(k: ImageKey, path: String) {
    map += (k -> path)
  }
  private def toURL(str: String): Option[URL] =
    tryoo { Option(zProject.classLoader.getResource(str)) }

  private def getOrFile(imageKey: ImageKey): Option[URL] =
    map.get(imageKey).flatMap(toURL) orElse getFile(imageKey)
  private def getFile(imageKey: ImageKey): Option[URL] =
    tryoo {
      Option(zProject.classLoader.getResource(imageKey.toPng)) orElse
        Option(zProject.classLoader.getResource(imageKey.toGif))
    }
  def findImage(imageKey: ImageKey): Option[URL] =
    getOrFile(imageKey) orElse { imageKey.moreGeneral flatMap { findImage } }
}
