package org.zaluum.nide.eclipse


import org.zaluum.nide.compiler._
import javax.swing.JComponent
import java.net.URLClassLoader
import java.net.URL
import org.eclipse.core.resources.{ IProject, IFile, IResource }
import org.eclipse.core.runtime.{ Path, IPath }
import org.eclipse.jdt.core.{ IJavaElement, IType, IAnnotatable, IJavaProject, IAnnotation, IMemberValuePair, IClasspathEntry }
import org.eclipse.jdt.core.search.{ SearchEngine, SearchPattern, SearchRequestor, SearchMatch, IJavaSearchConstants, TypeReferenceMatch }
import org.eclipse.jdt.internal.core.JavaModelManager
import scala.util.control.Exception._

trait ClassPath {
  def getResource(str:String):Option[URL]
}
class EclipseBoxClasspath(project: IProject) extends EclipseUtils with ClassPath with Scope{
  var cacheType = Map[Name,Type]()
  var creatorCache = Map[Name, () ⇒ JComponent]()
  def jmodel = JavaModelManager.getJavaModelManager.getJavaModel
  def jproject = jmodel.getJavaProject(project);
  val classLoader = {
    val urls = jproject.getResolvedClasspath(true) flatMap { e ⇒ pathToURL(e.getPath) }
    new URLClassLoader(urls, currentThread.getContextClassLoader)
  }
  case object root extends Symbol {
    val owner = NoSymbol
    val name = null
    scope = EclipseBoxClasspath.this
  }
  private def newJavaType(str:String) =
    (Name(str) -> new PrimitiveJavaType(root,Name(str)))
  var types = Map[Name, Type](newJavaType("double"))//TODO
  
  def lookupPort(name: Name): Option[Symbol] = None
  def lookupVal(name: Name): Option[Symbol] = None
  def lookupType(name: Name): Option[Type] = types.get(name)
  def lookupBoxType(name: Name): Option[Type] = cacheType.get(name)
  def lookupBoxTypeLocal(name: Name): Option[Type] = lookupBoxType(name)
  
  def boxes = cacheType.values
  def enter(sym: Symbol): Symbol = { throw new Exception("cannot enter new symbols to global scope") } 
  // cacheType += (sym.name->sym.asInstanceOf[Type]);sym}//throw new Exception("cannot enter")
  def update() {
    cacheType = cacheType.empty
    creatorCache = creatorCache.empty
    val searchScope = SearchEngine.createJavaSearchScope(
      Array[IJavaElement](jproject))
    val search = new SearchEngine()
    val pattern = SearchPattern.createPattern("org.zaluum.nide.java.Box",
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
    def forName(str: String): Option[Class[_]] = {
      try { Some(classLoader.loadClass(str)) }
      catch { case e: Exception ⇒ e.printStackTrace; None }
    }
    def processTypeSym(t:IType) {
      val fqn = Name(t.getFullyQualifiedName)
      val img = findAnnotations(t, t, "org.zaluum.nide.java.BoxImage").headOption flatMap { a ⇒
        findStringValueOfAnnotation(a, "value")
      }
      val creatorClass = findAnnotations(t, t, "org.zaluum.nide.java.Widget").headOption flatMap { a ⇒
        findStringValueOfAnnotation(a, "value")
      } flatMap { forName(_) }
      val bs = new BoxTypeSymbol(root, fqn, img) 
      bs.scope = this
      def pointOf(a: IAnnotation) = {
        val ox = findIntegerValueOfAnnotation(a, "x")
        val oy = findIntegerValueOfAnnotation(a, "y")
        (ox, oy) match {
          case (Some(x), Some(y)) ⇒ Point(x, y)
          case _ ⇒ Point(0, 0)
        }
      }
      def typeSignatureToName(str:String ) = str match{
        case "D" => Name("double")
        case _ => Name(str.dropRight(1).drop(1).replace('.', '/')) 
      }
      for (f ← t.getFields) {
        def port(in:Boolean,a:IAnnotation) {
          val port = new PortSymbol(bs,Name(f.getElementName),pointOf(a),if (in) In else Out)
          val tpe = lookupType(typeSignatureToName(f.getTypeSignature)) getOrElse (throw new Exception("tpe not found"))
          port.tpe = tpe
          bs.enter(port)           
        }
        findAnnotations(t, f, "org.zaluum.nide.java.In") foreach { port(true,_) }
        findAnnotations(t, f, "org.zaluum.nide.java.Out") foreach { port(false,_) }
      }
      cacheType += (bs.name -> bs)
      //creatorClass foreach { c ⇒ creatorCache += (bc.className, () ⇒ c.newInstance.asInstanceOf[JComponent]) }
      
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

