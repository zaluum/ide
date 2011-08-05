package org.zaluum.nide.eclipse

import scala.collection.mutable.Buffer

import org.eclipse.core.runtime.IProgressMonitor
import org.eclipse.core.runtime.NullProgressMonitor
import org.eclipse.jdt.core.search.IJavaSearchConstants
import org.eclipse.jdt.core.search.IJavaSearchScope
import org.eclipse.jdt.core.search.SearchEngine
import org.eclipse.jdt.core.search.SearchMatch
import org.eclipse.jdt.core.search.SearchPattern
import org.eclipse.jdt.core.search.SearchRequestor
import org.eclipse.jdt.core.search.TypeDeclarationMatch
import org.eclipse.jdt.core.search.TypeReferenceMatch
import org.eclipse.jdt.core.Flags
import org.eclipse.jdt.core.IAnnotatable
import org.eclipse.jdt.core.IAnnotation
import org.eclipse.jdt.core.IJavaElement
import org.eclipse.jdt.core.IJavaProject
import org.eclipse.jdt.core.IMember
import org.eclipse.jdt.core.IMemberValuePair
import org.eclipse.jdt.core.IMethod
import org.eclipse.jdt.core.IType
import org.zaluum.nide.compiler.Point
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
  def abstractMethodsOf(t: IType) = {
    visibleMethodsOf(t).filter(isAbstract(_))
  }
  def publicMethodsOf(t: IType) = {
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
      case _                  ⇒ Point(0, 0)
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

  def search(pattern: SearchPattern, searchScope: IJavaSearchScope, monitor: IProgressMonitor = null): List[IType] = {
    val search = new SearchEngine()
    val participants = Array(SearchEngine.getDefaultSearchParticipant())
    var list = List[IType]()
    val searchRequestor = new SearchRequestor() {
      def acceptSearchMatch(matchh: SearchMatch) {
          def processElement(a: AnyRef) = a match {
            case t: IType ⇒ list = t :: list
            case other    ⇒
          }
        matchh match {
          case t: TypeReferenceMatch   ⇒ processElement(t.getElement)
          case t: TypeDeclarationMatch ⇒ processElement(t.getElement)
        }
      }
    }
    search.search(pattern, participants, searchScope, searchRequestor, monitor)
    list
  }
  def projectScope(jproject: IJavaProject) =
    SearchEngine.createJavaSearchScope(Array[IJavaElement](jproject), IJavaSearchScope.APPLICATION_LIBRARIES | IJavaSearchScope.SOURCES | IJavaSearchScope.REFERENCED_PROJECTS)

  def sourcesScope(jproject: IJavaProject) =
    SearchEngine.createJavaSearchScope(Array[IJavaElement](jproject), IJavaSearchScope.SOURCES | IJavaSearchScope.APPLICATION_LIBRARIES)

  def classAndInterface(str: String) = SearchPattern.createPattern(str,
    IJavaSearchConstants.CLASS_AND_INTERFACE,
    IJavaSearchConstants.DECLARATIONS,
    SearchPattern.R_EXACT_MATCH)
  def patternAnnotation(str: String) = SearchPattern.createPattern(str,
    IJavaSearchConstants.ANNOTATION_TYPE,
    IJavaSearchConstants.ANNOTATION_TYPE_REFERENCE,
    SearchPattern.R_EXACT_MATCH)
}