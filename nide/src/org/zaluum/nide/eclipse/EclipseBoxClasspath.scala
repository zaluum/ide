package org.zaluum.nide.eclipse

import org.eclipse.jdt.core.IMemberValuePair
import org.eclipse.jdt.core.IAnnotation
import org.zaluum.nide.model.Point
import org.zaluum.nide.model.TypedPort
import org.eclipse.jdt.core.search.TypeReferenceMatch
import org.eclipse.jdt.core.IJavaProject
import org.eclipse.jdt.core.{IType,IAnnotatable}
import org.eclipse.jdt.internal.core.JavaModelManager
import org.eclipse.jdt.core.search.IJavaSearchConstants
import org.eclipse.jdt.core.IJavaElement
import org.eclipse.jdt.core.search.SearchMatch
import org.eclipse.jdt.core.search.SearchRequestor
import org.eclipse.jdt.core.search.SearchPattern
import org.eclipse.jdt.core.search.SearchEngine
import org.eclipse.core.resources.IProject
import org.eclipse.core.runtime.IProgressMonitor
import java.net.URL
import org.zaluum.nide.model.BoxClass
import org.zaluum.nide.compiler.ScannedBoxClassPath

class EclipseBoxClasspath(project: IProject) extends ScannedBoxClassPath {
  var cache = Map[String, BoxClass]()
  findAnnotated(null)
  protected def findAnnotated(monitor: IProgressMonitor) {
    val searchScope = SearchEngine.createJavaSearchScope(
      Array[IJavaElement](jproject))
    val search = new SearchEngine()
    val pattern = SearchPattern.createPattern("org.zaluum.nide.java.Box",
      IJavaSearchConstants.ANNOTATION_TYPE,
      IJavaSearchConstants.ANNOTATION_TYPE_REFERENCE,
      SearchPattern.R_EXACT_MATCH)
    val participants = Array(SearchEngine.getDefaultSearchParticipant())
    
    def isAnnType(t:IType, a:IAnnotation, str:String) = {
      val names = t.resolveType(a.getElementName)
      if (names==null) 
        false 
      else  {
        names exists { posible => (posible(0) + "." + posible(1)) == str }
      }
    }
    def findAnnotations(t:IType, ann:IAnnotatable, str:String) = ann.getAnnotations find { a => isAnnType(t,a,str) }
     
    def findStringValueOfAnnotation(a: IAnnotation, key: String) = {
      a.getMemberValuePairs find { v ⇒
        v.getMemberName == key && v.getValueKind == IMemberValuePair.K_STRING
      } map { _.getValue.asInstanceOf[String] }
    }
    def findIntegerValueOfAnnotation(a:IAnnotation, key:String) = {
      a.getMemberValuePairs find { v ⇒
        v.getMemberName == key && v.getValueKind == IMemberValuePair.K_INT
      } map { _.getValue.asInstanceOf[Int] }
    }
    def processType(t: IType) {
      val fqn = t.getFullyQualifiedName;
      val img = findAnnotations(t,t, "org.zaluum.nide.java.Box").headOption flatMap  { a=> 
        findStringValueOfAnnotation(a, "image")
      }

      val bc = new BoxClass(fqn, false, img.getOrElse(""))
      def pointOf (a:IAnnotation) = {
        val ox = findIntegerValueOfAnnotation(a,"x")
        val oy = findIntegerValueOfAnnotation(a,"y")
        (ox,oy) match {
          case (Some(x),Some(y)) => Point(x,y)
          case _ => Point(0,0)
        }
      }
      for (f ← t.getFields) {
        findAnnotations(t,f, "org.zaluum.nide.java.In") foreach { a ⇒
          bc.ports += TypedPort(f.getTypeSignature, true, f.getElementName, pointOf(a)) // FIXME 
        }
        findAnnotations(t,f, "org.zaluum.nide.java.Out") foreach { a ⇒
          bc.ports += TypedPort(f.getTypeSignature, false, f.getElementName, pointOf(a)) // FIXME 
        }
      }
      cache += (bc.className -> bc)
    }
    val searchRequestor = new SearchRequestor() {
      def acceptSearchMatch(matchh: SearchMatch) {
        matchh match {
          case t: TypeReferenceMatch ⇒
            t.getElement match {
              case t: IType ⇒ processType(t)
              case other ⇒ println("ohter match " + other + " " + other.getClass)
            }
        }
      }
    }
    search.search(pattern, participants, searchScope, searchRequestor, monitor)
  }
  def jmodel = JavaModelManager.getJavaModelManager.getJavaModel
  def jproject = jmodel.getJavaProject(project);

  def boxClasses: Set[BoxClass] = { cache.values.toSet }
  def find(str: String): Option[BoxClass] = cache.get(str)
  def getResource(str: String): Option[URL] = {
    val cpaths = jproject.getResolvedClasspath(true)
    if (str=="") return None
    for (c<-cpaths) {
      val path = c.getPath.makeAbsolute
      if (path.getFileExtension == "jar") {
        val url = new URL ("jar:" + path.toFile.toURI.toURL.toString + "!/" + str)
        try{
          val c = url.openConnection.asInstanceOf[java.net.JarURLConnection]
          c.connect()
          return Some(url)
        }catch {
          case e:java.io.IOException => 
        }
      }else {
        val file = new java.io.File(path + "/" + str)
        if (file.exists)  return Some(file.toURI.toURL)
      }
    }
    return None
  }
}