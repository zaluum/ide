package org.zaluum.nide.eclipse

import java.io.InputStream
import org.zaluum.nide.protobuf.BoxFileProtos
import java.net.{URL, URLClassLoader}
import org.eclipse.core.runtime.IPath
import org.eclipse.jdt.core.search.SearchPattern
import org.eclipse.jdt.core.{IType, IJavaProject, IAnnotation, IClasspathEntry, Flags}
import org.zaluum.nide.compiler._
import org.zaluum.runtime.{BoxImage, Box}

class ZaluumLoader(val zProject : ZaluumProject) extends EclipseUtils{
  import AnnotationUtils._
  import SearchUtils._
  def jProject = zProject.jProject
  
  def searchJavaType(name: Name): Option[JavaType] = 
    zProject.primitives.find(name).orElse(searchNonPrimitiveJavaType(name))

  def searchNonPrimitiveJavaType(name: Name): Option[JavaType] =
    search(classAndInterface(name.str), jProject) { t ⇒ toJavaType(name) } headOption
  
  def searchBoxType(name: Name): Option[BoxTypeSymbol] =
    searchSourceZaluum(name.str) orElse {
      search(classAndInterface(name.str),jProject) { t ⇒ toBoxTypeSymbol(t) } headOption
    }

  def index : Seq[Name] = indexZaluum ++ indexJava
  private def indexZaluum = for (f <- visitSourceZaluums;cl <- toClassName(f)) yield cl
  private def indexJava = search(patternAnnotation(classOf[Box].getName),jProject) { 
     t=> Name(t.getFullyQualifiedName)
  }
  
  private def toJavaType(t: IType): JavaType = new JavaType(zProject, Name(t.getElementName)) 
  private def toJavaType(name: Name): JavaType = new JavaType(zProject, name)
  def toBoxTypeSymbol(t: IType): BoxTypeSymbol = {
    val fqn = Name(t.getFullyQualifiedName)
    val img = findAnnotations(t, t, classOf[BoxImage].getName).headOption flatMap { a ⇒
      findStringValueOfAnnotation(a, "value")
    }
    val guiClass = t.getFields.find { f ⇒ f.getElementName == "_widget" }.flatMap { f ⇒
      signatureToName(t, f.getTypeSignature)
    }
    val superName = signatureToName(t, t.getSuperclassTypeSignature)
    println(fqn + " supername " + superName)
    val bs = new BoxTypeSymbol(zProject, fqn, superName, img, guiClass, Flags.isAbstract(t.getFlags()))
    bs.scope = zProject
    
    for (f ← t.getFields) {
      val name = Name(f.getElementName)
      val tpeName = signatureToName(t, f.getTypeSignature)
      val tpe = (for (n ← tpeName; jt ← zProject.lookupType(n)) yield jt).getOrElse { NoSymbol }
      def port(in: Boolean, a: IAnnotation) {
        val port = new PortSymbol(bs, Name(f.getElementName), annotationToPoint(a), if (in) In else Out)
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
  private def shallowAnalize(t:BoxDef) : Option[BoxTypeSymbol]= {
    val scope = new FakeGlobalScope(zProject)
    val reporter = new Reporter()
    val analyzedTree = new Analyzer(reporter, t, scope).shallowCompile()
    analyzedTree.symbol match {
      case b:BoxTypeSymbol => Some(b)
      case _ => None
    }
  }
  private def zaluumToSymbol(i:InputStream,name:Name) : Option[BoxTypeSymbol] = {
    try{
        shallowAnalize(ZaluumBuilder.readTree(i,name))
    }catch {
      case e => 
        e.printStackTrace; 
        None
    }
  }
  def searchSourceZaluum(dotName:String) : Option[BoxTypeSymbol]= {
    sourcePaths.view.flatMap{ path => 
        val filePath = path.append(dotName.replace('.','/')).addFileExtension("zaluum")
        pathToURL(filePath) flatMap { url =>
          val i =url.openStream
          try {
            zaluumToSymbol(i,Name(dotName))
          }finally(i.close)
        } 
    }.headOption
  }
}