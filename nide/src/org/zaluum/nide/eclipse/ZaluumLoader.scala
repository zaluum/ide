package org.zaluum.nide.eclipse

import org.eclipse.jdt.core.Signature
import org.eclipse.core.runtime.NullProgressMonitor
import org.eclipse.jdt.core.dom.AST
import java.io.InputStream
import org.zaluum.nide.protobuf.BoxFileProtos
import java.net.{ URL, URLClassLoader }
import org.eclipse.core.runtime.IPath
import org.eclipse.jdt.core.search.SearchPattern
import org.eclipse.jdt.core.{ IType, IMethod, IJavaProject, IAnnotation, IClasspathEntry, Flags }
import org.zaluum.nide.compiler._
import org.zaluum.annotation.{ BoxImage, Box }

class ZaluumLoader(val zProject: ZaluumProject) extends EclipseUtils {
  import AnnotationUtils._
  import SearchUtils._
  def jProject = zProject.jProject

  def searchJavaType(name: Name): Option[JavaType] =
    zProject.primitives.find(name).orElse(searchNonPrimitiveJavaType(name))

  def searchNonPrimitiveJavaType(name: Name): Option[JavaType] =
    search(classAndInterface(name.str), jProject) { t ⇒ toJavaType(name) } headOption

  def searchBoxType(name: Name): Option[BoxTypeSymbol] =
    searchSourceZaluum(name.str) orElse {
      search(classAndInterface(name.str), jProject) { t ⇒ toBoxTypeSymbol(t) } headOption
    }

  def index: Seq[Name] = indexZaluum ++ indexJava
  private def indexZaluum = for (f ← allSourceZaluums; cl ← toClassName(f)) yield cl
  private def indexJava = search(patternAnnotation(classOf[Box].getName), jProject) { t ⇒
    Name(t.getFullyQualifiedName)
  }

  private def toJavaType(t: IType): JavaType = new JavaType(zProject, Name(t.getElementName))
  private def toJavaType(name: Name): JavaType = new JavaType(zProject, name)
  private def signatureToType(t: IType, sig: String) = {
    val tpeName = signatureToName(t, sig)
    (for (n ← tpeName; jt ← zProject.lookupType(n)) yield jt).getOrElse { NoSymbol }

  }
  
  def toBoxTypeSymbol(t: IType): BoxTypeSymbol = { 
    val fqn = Name(t.getFullyQualifiedName)
    val img = findAnnotations(t, t, classOf[BoxImage].getName).headOption flatMap { a ⇒
      findStringValueOfAnnotation(a, "value")
    }
    val guiClass = t.getFields.find { f ⇒ f.getElementName == "_widget" }.flatMap { f ⇒
      signatureToName(t, f.getTypeSignature)
    }
    val superName = signatureToName(t, t.getSuperclassTypeSignature)

    val bs = new BoxTypeSymbol(zProject, fqn, superName, img, guiClass, Flags.isAbstract(t.getFlags()))
    bs.scope = zProject
    // constructors
    def isPrivateDefaultConstructor(c:IMethod) = {
      c.isConstructor && c.getParameterNames.isEmpty && (Flags.isPrivate(c.getFlags) || Flags.isProtected(c.getFlags))
    }
    val defaultCons = if (t.getMethods exists (isPrivateDefaultConstructor(_)))
      None
       else 
       Some(new Constructor(bs,List())) 
    val collectedConstructors =
      for (
        c ← t.getMethods.toList;
        if !Flags.isAbstract(t.getFlags) &&
          c.isConstructor &&
          Flags.isPublic(c.getFlags)
      ) yield {
        val params = c.getParameterNames.toList.zip(c.getParameterTypes)
        new Constructor(bs,
          for ((name, sig) ← params) yield {
            val param = new ParamSymbol(bs, Name(name))
            param.tpe = signatureToType(t, sig)
            param
          })
      }
    bs.constructors = if (collectedConstructors exists {_.params.isEmpty}) 
      collectedConstructors
    else
      collectedConstructors ++ defaultCons.toList 
    // abstract methods
    val abstracts = JDTUtils.abstractMethodsOf(t)
    def isOverridable(m:IMethod) = {
      m.getElementName == "contents" && m.getParameterTypes.size==0 && m.getReturnType== Signature.SIG_VOID// TODO remove string
    }
    if (abstracts.size == 1 && isOverridable(abstracts.head)) { 
      bs.okOverride = true
    }
    // setters
    val methods = JDTUtils.publicMethodsOf(t)
    val setters = methods filter {m => m.getElementName.startsWith("set") && m.getReturnType == Signature.SIG_VOID && m.getParameterTypes.size==1}
    for (m<-setters) {
      val name = m.getElementName
      val param = new ParamSymbol(bs, Name(name))
      val tpe = signatureToType(t,m.getParameterTypes.head)
      param.tpe = tpe
      bs.enter(param)
    }
    // ports
    for (f ← t.getFields) {
      val name = Name(f.getElementName)
      val tpe = signatureToType(t, f.getTypeSignature)
      def port(in: Boolean, a: IAnnotation) {
        val port = new PortSymbol(bs, Name(f.getElementName), annotationToPoint(a), if (in) In else Out)
        port.tpe = tpe
        bs.enter(port)
      }
      findAnnotations(t, f, classOf[org.zaluum.annotation.In].getName) foreach { port(true, _) }
      findAnnotations(t, f, classOf[org.zaluum.annotation.Out].getName) foreach { port(false, _) }
    }
    bs
  }

  private def signatureToName(t: IType, signature: String): Option[Name] = {
    zProject.primitives.find(signature).map { _.name }.orElse {
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
  }
  private def shallowAnalize(t: BoxDef): Option[BoxTypeSymbol] = {
    val scope = new FakeGlobalScope(zProject)
    val reporter = new Reporter()
    val analyzedTree = new Analyzer(reporter, t, scope).shallowCompile()
    analyzedTree.symbol match {
      case b: BoxTypeSymbol ⇒ Some(b)
      case _ ⇒ None
    }
  }
  private def zaluumToSymbol(i: InputStream, name: Name): Option[BoxTypeSymbol] = {
    try {
      shallowAnalize(ZaluumBuilder.readTree(i, name))
    } catch {
      case e ⇒
        e.printStackTrace;
        None
    }
  }
  def searchSourceZaluum(dotName: String): Option[BoxTypeSymbol] = {
    sourcePaths.view.flatMap { path ⇒
      val filePath = path.append(dotName.replace('.', '/')).addFileExtension("zaluum")
      pathToURL(filePath) flatMap { url ⇒
        val i = url.openStream
        try {
          zaluumToSymbol(i, Name(dotName))
        } finally (i.close)
      }
    }.headOption
  }
}