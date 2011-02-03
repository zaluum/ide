package org.zaluum.nide.eclipse

import org.zaluum.nide.newcompiler.LocalScope
import org.zaluum.nide.newcompiler.PrimitiveJavaType
import org.zaluum.nide.newcompiler.Type
import org.zaluum.nide.newcompiler.Scope
import org.zaluum.nide.newcompiler.PortSymbol
import org.zaluum.nide.newcompiler.BoxTypeSymbol
import org.zaluum.nide.newcompiler.{Scope,Symbol,NoSymbol}
import org.zaluum.nide.newcompiler.Name
import org.zaluum.nide.compiler.TypedPort
import org.zaluum.nide.compiler.BoxClass
import org.zaluum.nide.compiler.PresentationCompiler
import org.zaluum.nide.model.InnerBoxClassName
import org.zaluum.nide.model.ExtBoxClassName
import org.zaluum.nide.model.BoxClassName
import javax.swing.JComponent
import java.net.URLClassLoader
import java.net.URL
import org.eclipse.core.resources.{ IProject, IFile, IResource }
import org.eclipse.core.runtime.{ Path, IPath }
import org.eclipse.jdt.core.{ IJavaElement, IType, IAnnotatable, IJavaProject, IAnnotation, IMemberValuePair, IClasspathEntry }
import org.eclipse.jdt.core.search.{ SearchEngine, SearchPattern, SearchRequestor, SearchMatch, IJavaSearchConstants, TypeReferenceMatch }
import org.eclipse.jdt.internal.core.JavaModelManager
import org.zaluum.nide.compiler.ScannedBoxClassPath
import org.zaluum.nide.model.{ Point, ProtoBuffers, BoxClassDecl , Dimension}
import scala.util.control.Exception._

class EclipseBoxClasspath(project: IProject) extends ScannedBoxClassPath with EclipseUtils with Scope{
  var cache = Map[BoxClassName, BoxClass]()
  var cacheType = Map[Name,Type]()
  var creatorCache = Map[BoxClassName, () ⇒ JComponent]()
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
  private def newJavaType(str: String) =
    (Name(str) -> new PrimitiveJavaType(root,Name(str)))
  var types = Map[Name, Type](newJavaType("D"))//TODO
  
  def lookupPort(name: Name): Option[Symbol] = None
  def lookupVal(name: Name): Option[Symbol] = None
  def lookupType(name: Name): Option[Type] = types.get(name)
  def lookupBoxType(name: Name): Option[Type] = cacheType.get(name)
  
  def enter(sym: Symbol): Symbol = { throw new Exception("cannot enter new symbols to global scope") } 
  // cacheType += (sym.name->sym.asInstanceOf[Type]);sym}//throw new Exception("cannot enter")
  def update() {
    cache = cache.empty
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
      val bs = new BoxTypeSymbol(root, fqn) // TODO image
      bs.scope = this
      //val bc = new BoxClass(fqn, false, img.getOrElse(""), creatorClass.isDefined)
      def pointOf(a: IAnnotation) = {
        val ox = findIntegerValueOfAnnotation(a, "x")
        val oy = findIntegerValueOfAnnotation(a, "y")
        (ox, oy) match {
          case (Some(x), Some(y)) ⇒ Point(x, y)
          case _ ⇒ Point(0, 0)
        }
      }
      for (f ← t.getFields) {
        def port(in:Boolean,a:IAnnotation) {
          val port = new PortSymbol(bs,Name(f.getElementName),pointOf(a),in) 
          port.tpe = lookupType(Name(f.getTypeSignature)) getOrElse {NoSymbol}
          println("entering port in scope " + f.getElementName + " " )
          bs.enter(port)           
        }
        findAnnotations(t, f, "org.zaluum.nide.java.In") foreach { port(true,_) }
        findAnnotations(t, f, "org.zaluum.nide.java.Out") foreach { port(false,_) }
      }
      cacheType += (bs.name -> bs)
      //creatorClass foreach { c ⇒ creatorCache += (bc.className, () ⇒ c.newInstance.asInstanceOf[JComponent]) }
      
    }
    def processType(t: IType) {
      val fqn = BoxClassName.parse(t.getFullyQualifiedName)
      val img = findAnnotations(t, t, "org.zaluum.nide.java.BoxImage").headOption flatMap { a ⇒
        findStringValueOfAnnotation(a, "value")
      }
      val creatorClass = findAnnotations(t, t, "org.zaluum.nide.java.Widget").headOption flatMap { a ⇒
        findStringValueOfAnnotation(a, "value")
      } flatMap { forName(_) }
      val bc = new BoxClass(fqn, false, img.getOrElse(""), creatorClass.isDefined)
      def pointOf(a: IAnnotation) = {
        val ox = findIntegerValueOfAnnotation(a, "x")
        val oy = findIntegerValueOfAnnotation(a, "y")
        (ox, oy) match {
          case (Some(x), Some(y)) ⇒ Point(x, y)
          case _ ⇒ Point(0, 0)
        }
      }
      for (f ← t.getFields) {
        findAnnotations(t, f, "org.zaluum.nide.java.In") foreach { a ⇒
          bc.ports += TypedPort(f.getTypeSignature, true, f.getElementName, pointOf(a))
        }
        findAnnotations(t, f, "org.zaluum.nide.java.Out") foreach { a ⇒
          bc.ports += TypedPort(f.getTypeSignature, false, f.getElementName, pointOf(a))
        }
      }
      cache += (bc.className -> bc)
      creatorClass foreach { c ⇒ creatorCache += (bc.className, () ⇒ c.newInstance.asInstanceOf[JComponent]) }

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
    def addCache(cl: BoxClass) {
      cache += (cl.className -> cl)
      cl.innerClasses foreach { addCache(_) }
    }
    
    toClassName(f) foreach { name ⇒
      val decl = ProtoBuffers.readBoxClassDecl(f.getContents, name)
      val boxClass =PresentationCompiler.toBoxClass(decl)
      addCache(boxClass)
      if (boxClass.visual) // FIXME inner classes visualizations
        creatorCache += ( name -> (()=>PresentationCompiler.createGUI(decl,this)))
    }
  }
  def boxClasses: Set[BoxClass] = { cache.values.toSet }
  def find(name: BoxClassName): Option[BoxClass] = cache.get(name)
  def findGuiCreator(name: BoxClassName): Option[() ⇒ JComponent] = creatorCache.get(name)

  def toClassName(f: IFile): Option[BoxClassName] = {
    val path = f.getFullPath
    val oSourcePath = sourcePaths.find(_.isPrefixOf(path))
    oSourcePath map { sourcePath ⇒
      val relativePath = path.removeFirstSegments(sourcePath.segmentCount)
      val result = relativePath.segments.reduceLeft(_ + "." + _)
      Option(relativePath.getFileExtension) match {
        case Some(str: String) ⇒ result.dropRight(str.length + 1)
        case None ⇒ result
      }
    } map { ExtBoxClassName(_) }
  }
  def pathToURL(path: IPath): Option[URL] = {
    Option(workspaceRoot.findMember(path)) map { p ⇒
      p.getLocationURI.toURL
    } orElse {
      val f = path.toFile;
      if (f.exists) Some(f.toURI.toURL) else None
    }
  }

  def jarURL(jarPath: IPath, filePath: String): Option[URL] = filePath match {
    case "" ⇒ None
    case _ ⇒
      pathToURL(jarPath) flatMap { jarURL ⇒
        val url = new URL("jar:" + jarURL + "!/" + filePath)
        catching(classOf[java.io.IOException]) opt {
          val c = url.openConnection.asInstanceOf[java.net.JarURLConnection]
          c.connect()
          url
        }
      }
  }
  def workspaceRoot = project.getWorkspace.getRoot
  def pathFileToURL(path: IPath, file: String) = file match {
    case "" ⇒ None
    case _ ⇒
      path.getFileExtension match {
        case "jar" ⇒ jarURL(path, file)
        case _ ⇒ pathToURL(path.append(new Path(file)))
      }
  }
  def getResource(str: String): Option[URL] = {
    val cpaths = jproject.getResolvedClasspath(true)
    cpaths.view.flatMap { cp ⇒ pathFileToURL(cp.getPath, str) } headOption
  }
}