package org.zaluum.nide.eclipse

import org.zaluum.runtime.Box
import org.zaluum.runtime.BoxImage
import org.eclipse.jdt.core.search.TypeDeclarationMatch
import org.zaluum.nide.compiler._
import javax.swing.JComponent
import java.net.URLClassLoader
import java.net.URL
import org.eclipse.core.resources.{ IProject, IFile, IResource }
import org.eclipse.core.runtime.{ Path, IPath }
import org.eclipse.jdt.core.{ IJavaElement, IType, IAnnotatable, IJavaProject, IAnnotation, IMemberValuePair, IClasspathEntry, Flags }
import org.eclipse.jdt.core.search.{ SearchEngine, SearchPattern, SearchRequestor, SearchMatch, IJavaSearchConstants, TypeReferenceMatch }
import org.eclipse.jdt.internal.core.JavaModelManager
import scala.util.control.Exception._

trait ClassPath {
  def getResource(str: String): Option[URL]
}
class EclipseBoxClasspath(project: IProject) extends EclipseUtils with ClassPath with Scope {
  var cacheType = Map[Name, BoxTypeSymbol]()
  var creatorCache = Map[Name, () ⇒ JComponent]()
  def jmodel = JavaModelManager.getJavaModelManager.getJavaModel
  def jproject = jmodel.getJavaProject(project);
  val classLoader = {
    val urls = jproject.getResolvedClasspath(true) flatMap { e ⇒ pathToURL(e.getPath) }
    new URLClassLoader(urls, Thread.currentThread.getContextClassLoader)
  }
  case object root extends Symbol {
    val owner = NoSymbol
    val name = null
    scope = EclipseBoxClasspath.this
  }
  object primitives {
    private def n(str: String, desc: String) = new PrimitiveJavaType(root, Name(str), desc)
    val byte = n("byte", "B")
    val short = n("short", "S")
    val int = n("int", "I")
    val long = n("long", "J")
    val float = n("float", "F")
    val double = n("double", "D")
    val boolean = n("boolean", "Z")
    val char = n("char", "C")
    val allTypes = List(byte, short, int, long, float, double, boolean, char)
    def find(desc: String) = allTypes.find(_.descriptor == desc)
  }

  var types: Map[Name, JavaType] = primitives.allTypes map { t ⇒ (t.name -> t) } toMap
  def newJavaType(n: Name) = {
    val p = new JavaType(root, n)
    types += (n -> p)
    p
  }
  def lookupPort(name: Name): Option[Symbol] = None
  def lookupVal(name: Name): Option[Symbol] = None
  def lookupType(name: Name): Option[Type] = {
    types.get(name) orElse {
      val searchScope = SearchEngine.createJavaSearchScope(Array[IJavaElement](jproject))
      val search = new SearchEngine()
      val pattern = SearchPattern.createPattern(name.str,
        IJavaSearchConstants.CLASS_AND_INTERFACE,
        IJavaSearchConstants.DECLARATIONS,
        SearchPattern.R_EXACT_MATCH)
      val participants = Array(SearchEngine.getDefaultSearchParticipant())
      var found = false
      val searchRequestor = new SearchRequestor() {
        def acceptSearchMatch(matchh: SearchMatch) {
          matchh match {
            case t: TypeDeclarationMatch ⇒
              t.getElement match {
                case t: IType ⇒ println("found type" + t); found = true
                case other ⇒ println("ohter match " + other + " " + other.getClass)
              }
          }
        }
      }
      search.search(pattern, participants, searchScope, searchRequestor, null)
      if (found) Some(newJavaType(name)) else None
      /*val cl = classLoader.loadClass(name.toString)
        val tpe = newJavaType(name.toString)
        types += tpe
        Some(tpe._2)
      } catch {
        case e: ClassNotFoundException ⇒ None
      }*/
    }
  }
  def lookupBoxType(name: Name): Option[Type] = cacheType.get(name)
  def lookupBoxTypeLocal(name: Name): Option[Type] = lookupBoxType(name)

  def boxes = cacheType.values
  def enter[S <: Symbol](sym: S): S = { throw new Exception("cannot enter new symbols to global scope") }
  // cacheType += (sym.name->sym.asInstanceOf[Type]);sym}//throw new Exception("cannot enter")
  def update() {
    cacheType = cacheType.empty
    creatorCache = creatorCache.empty
    val searchScope = SearchEngine.createJavaSearchScope(
      Array[IJavaElement](jproject))
    val search = new SearchEngine()
    val pattern = SearchPattern.createPattern(classOf[Box].getName,
      IJavaSearchConstants.ANNOTATION_TYPE,
      IJavaSearchConstants.ANNOTATION_TYPE_REFERENCE,
      SearchPattern.R_EXACT_MATCH)
    val participants = Array(SearchEngine.getDefaultSearchParticipant())

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
    def findAnnotations(t: IType, ann: IAnnotatable, str: String) = ann.getAnnotations find { a ⇒ isAnnType(t, a, str) }

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

    def processTypeSym(t: IType) {
      def resolveTypeName(signature: String): Option[Name] =
        primitives.find(signature).map { _.name }.orElse {
          signature match {
            case null ⇒ None
            case _ ⇒
              val className = signature.dropRight(1).drop(1).replace('/', '.')
              val res = Option(t.resolveType(className))
              res flatMap {
                _.headOption map { arr ⇒ Name(arr.mkString(".")) }
              }
          }
        }
      val fqn = Name(t.getFullyQualifiedName)
      println("processing box: " + fqn)
      val img = findAnnotations(t, t, classOf[BoxImage].getName).headOption flatMap { a ⇒
        findStringValueOfAnnotation(a, "value")
      }
      val guiClass = t.getFields.find { f ⇒ f.getElementName == "_widget" }.flatMap { f ⇒
        println("found widget " + f.getTypeSignature);
        resolveTypeName(f.getTypeSignature)
      }
      val superName = resolveTypeName(t.getSuperclassTypeSignature)
      val bs = new BoxTypeSymbol(root, fqn, superName, img, guiClass, Flags.isAbstract(t.getFlags()))
      bs.scope = this
      def pointOf(a: IAnnotation) = {
        val ox = findIntegerValueOfAnnotation(a, "x")
        val oy = findIntegerValueOfAnnotation(a, "y")
        (ox, oy) match {
          case (Some(x), Some(y)) ⇒ Point(x, y)
          case _ ⇒ Point(0, 0)
        }
      }
      for (f ← t.getFields) {
        val name = Name(f.getElementName)
        val tpeName = resolveTypeName(f.getTypeSignature)
        val tpe = tpeName.flatMap { lookupType(_) }.getOrElse { NoSymbol }
        def port(in: Boolean, a: IAnnotation) {
          println("resolving port " + f + " " + f.getTypeSignature)
          val port = new PortSymbol(bs, Name(f.getElementName), pointOf(a), if (in) In else Out)
          port.tpe = tpe
          bs.enter(port)
        }
        def param(a: IAnnotation) {
          val param = new ParamSymbol(bs, Name(f.getElementName), "", In) // TODO default
          param.tpe = tpe
          bs.enter(param)
        }
        findAnnotations(t, f, classOf[org.zaluum.runtime.In].getName) foreach { port(true, _) }
        findAnnotations(t, f, classOf[org.zaluum.runtime.Out].getName) foreach { port(false, _) }
        findAnnotations(t, f, classOf[org.zaluum.runtime.Param].getName) foreach { param(_) }
      }
      cacheType += (bs.name -> bs)

    }
    val searchRequestor = new SearchRequestor() {
      def acceptSearchMatch(matchh: SearchMatch) {
        matchh match {
          case t: TypeReferenceMatch ⇒
            t.getElement match {
              case t: IType ⇒ processTypeSym(t)
              case other ⇒ println("ohter match " + other + " " + other.getClass)
            }
        }
      }
    }
    // WORK
    search.search(pattern, participants, searchScope, searchRequestor, null)
    // FIND ZALUUMS IN SOURCE
    visitSourceZaluums { loadZaluum(_) }
    resolve()
  }
  def resolve() {
    for (t ← cacheType.values; superName ← t.superName) {
      t.superSymbol = cacheType.get(superName)
    }
  }
  def loadZaluum(f: IFile) {
    /*def addCache(cl: BoxClass) {
      cache += (cl.className -> cl)
      cl.innerClasses foreach { addCache(_) }
    }
    
    toClassName(f) foreach { name ⇒
      val decl = ProtoBuffers.readBoxClassDecl(f.getContents, name)
      val boxClass =PresentationCompiler.toBoxClass(decl)
      addCache(boxClass)
      if (boxClass.visual) // FIXME inner classes visualizations
        creatorCache += ( name -> (()=>PresentationCompiler.createGUI(decl,this)))
    }*/
  }
  def findGuiCreator(name: Name): Option[() ⇒ JComponent] = creatorCache.get(name)
}

