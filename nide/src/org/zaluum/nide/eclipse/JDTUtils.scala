package org.zaluum.nide.eclipse

import scala.collection.mutable.Buffer
import org.eclipse.jdt.core.IMember
import org.eclipse.jdt.core.IMethod
import org.eclipse.core.runtime.NullProgressMonitor
import org.eclipse.jdt.core.IElementChangedListener
import org.eclipse.jdt.core.JavaCore
import org.zaluum.annotation.Box
import org.zaluum.annotation.BoxImage
import org.eclipse.jdt.core.search.TypeDeclarationMatch
import org.zaluum.nide.compiler._
import javax.swing.JComponent
import java.net.URLClassLoader
import java.net.URL
import org.eclipse.core.resources.{ IProject, IFile, IResource }
import org.eclipse.core.runtime.{ Path, IPath }
import org.eclipse.jdt.core.{ ICompilationUnit, IJavaElement, IJavaElementDelta, IType, IAnnotatable, IJavaProject, IAnnotation, IMemberValuePair, IClasspathEntry, Flags, ElementChangedEvent }
import org.eclipse.jdt.core.search.{ SearchEngine, SearchPattern, SearchRequestor, SearchMatch, IJavaSearchConstants, TypeReferenceMatch }
import org.eclipse.jdt.internal.core.JavaModelManager
import scala.util.control.Exception._
object JDTUtils {
  def allMethodsOf(t: IType) = {
    val hier = t.newSupertypeHierarchy(new NullProgressMonitor)
    val supers = for (st ← hier.getAllSuperclasses(t).toList.view; m ← st.getMethods) yield m
    supers ++ t.getMethods
  }
  def visibleMethodsOf(t: IType) = {
    var methods = Buffer[IMethod]()
    def process(m: IMethod) {
      methods.filterNot(m.isSimilar(_))
      methods += m
    }
    for (m ← allMethodsOf(t)) process(m)
    methods.toList
  }
  def abstractMethodsOf (t:IType) = {
    visibleMethodsOf(t).filter(isAbstract(_))
  }
  def publicMethodsOf(t:IType) = {
    visibleMethodsOf(t).filter(isPublic(_))
  }
  def isAbstract(t: IMember) = Flags.isAbstract(t.getFlags)
  def isPublic(t: IMember) = Flags.isPublic(t.getFlags)
  def isPrivate(t: IMember) = Flags.isPrivate(t.getFlags)
  def isProtected(t: IMember) = Flags.isProtected(t.getFlags)
  def isPackageDefault(t: IMember) = Flags.isPackageDefault(t.getFlags)
}
object AnnotationUtils {
  def annotationToPoint(a: IAnnotation) = {
    val ox = findIntegerValueOfAnnotation(a, "x")
    val oy = findIntegerValueOfAnnotation(a, "y")
    (ox, oy) match {
      case (Some(x), Some(y)) ⇒ Point(x, y)
      case _ ⇒ Point(0, 0)
    }
  }
  def findAnnotations(t: IType, ann: IAnnotatable, str: String) =
    ann.getAnnotations find { a ⇒ isAnnType(t, a, str) }

  def findStringValueOfAnnotation(a: IAnnotation, key: String) = {
    a.getMemberValuePairs find { v ⇒
      v.getMemberName == key && v.getValueKind == IMemberValuePair.K_STRING
    } map { _.getValue.asInstanceOf[String] }
  }
  def findIntegerValueOfAnnotation(a: IAnnotation, key: String) = {
    a.getMemberValuePairs find { v ⇒
      v.getMemberName == key && v.getValueKind == IMemberValuePair.K_INT
    } map { _.getValue.asInstanceOf[Int] }
  }
  def isAnnType(t: IType, a: IAnnotation, str: String) = {
    val names = t.resolveType(a.getElementName)
    if (names == null) {
      false
    } else {
      names exists { posible ⇒
        val fullname = posible(0) + "." + posible(1)
        fullname == str
      }
    }
  }
}
object SearchUtils {
  def search[A](pattern: SearchPattern, jproject: IJavaProject)(processor: (IType ⇒ A)): List[A] = {
    val searchScope = SearchEngine.createJavaSearchScope(Array[IJavaElement](jproject))
    val search = new SearchEngine()
    val participants = Array(SearchEngine.getDefaultSearchParticipant())
    var list = List[A]()
    val searchRequestor = new SearchRequestor() {
      def acceptSearchMatch(matchh: SearchMatch) {
        def processElement(a: AnyRef) = a match {
          case t: IType ⇒ list = processor(t) :: list
          case other ⇒
        }
        matchh match {
          case t: TypeReferenceMatch ⇒ processElement(t.getElement)
          case t: TypeDeclarationMatch ⇒ processElement(t.getElement)
        }
      }
    }
    search.search(pattern, participants, searchScope, searchRequestor, null)
    list
  }
  def classAndInterface(str: String) = SearchPattern.createPattern(str,
    IJavaSearchConstants.CLASS_AND_INTERFACE,
    IJavaSearchConstants.DECLARATIONS,
    SearchPattern.R_EXACT_MATCH)
  def patternAnnotation(str: String) = SearchPattern.createPattern(str,
    IJavaSearchConstants.ANNOTATION_TYPE,
    IJavaSearchConstants.ANNOTATION_TYPE_REFERENCE,
    SearchPattern.R_EXACT_MATCH)
}