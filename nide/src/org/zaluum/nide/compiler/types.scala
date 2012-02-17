package org.zaluum.nide.compiler
import org.eclipse.jdt.internal.compiler.lookup.ArrayBinding
import org.eclipse.jdt.internal.compiler.lookup.BaseTypeBinding
import org.eclipse.jdt.internal.compiler.lookup.TypeBinding
import org.eclipse.jdt.internal.compiler.lookup.ReferenceBinding
import org.zaluum.nide.zge.Controller
import org.zaluum.nide.zge.ParamProperty
import org.zaluum.nide.eclipse.integration.model.ZaluumClassScope
import org.zaluum.nide.zge.BeanProperty
import org.eclipse.jdt.internal.compiler.lookup.MethodBinding

trait Type {
  def name: Name
}

trait JavaType extends Type {
  def fqName: Name
  def javaSize = 1
  type B >: Null <: TypeBinding
  def binding: B
  def name: Name
  def descriptor: String
  override def toString = "JavaType(" + name + ")"
  def loadClass(cl: ClassLoader): Option[Class[_]]
}
class PrimitiveJavaType(
    val name: Name,
    override val descriptor: String,
    override val javaSize: Int,
    val boxedName: Name,
    val boxMethod: String,
    val binding: BaseTypeBinding,
    javaclass: Class[_]) extends JavaType {
  type B = BaseTypeBinding
  val fqName = name
  def loadClass(cl: ClassLoader) = Some(javaclass)
}
class ArrayType(
    val of: JavaType,
    val dim: Int,
    val binding: ArrayBinding) extends JavaType {
  type B = ArrayBinding
  assert(!of.isInstanceOf[ArrayType])
  def descriptor = "[" * dim + of.descriptor
  def name = Name(of.fqName.str + "[]" * dim)
  def fqName = name
  override def equals(that: Any) = {
    that match {
      case a: ArrayType ⇒ a.of == of && a.dim == dim
      case _            ⇒ false
    }
  }
  override def hashCode = {
    41 * (41 + of.hashCode) + dim
  }
  def loadClass(cl: ClassLoader) = None
  override def toString = "ArrayType(" + of.toString + ", " + dim + ")"
}
class ClassJavaType(
  val binding: ReferenceBinding,
  val scope: ZaluumClassScope)
    extends JavaType with PropertySourceType with ClassJavaTypeFinder {
  type B = ReferenceBinding
  def descriptor = "L" + fqName.internal + ";"
  lazy val name = Name(binding.compoundName.last.mkString)
  lazy val pkg = Name(binding.fPackage.compoundName.map(_.mkString).mkString("."))
  lazy val fqName: Name = if (pkg.str != "") Name(pkg.str + "." + name.str) else name
  lazy val beanProperties = {
    val map = scala.collection.mutable.HashMap[String, (MethodBinding, MethodBinding)]()
    for (m ← allMethods(false, scope)) {
      if (MethodHelper.isGetter(m)) {
        val name = MethodHelper.propertyName(m)
        map.get(name) match {
          case Some((g, s)) ⇒ map(name) = (m, s)
          case _            ⇒ map(name) = (m, null)
        }
      } else if (MethodHelper.isSetter(m)) {
        val name = MethodHelper.propertyName(m)
        map.get(name) match {
          case Some((g, s)) ⇒ map(name) = (g, m)
          case _            ⇒ map(name) = (null, m)
        }
      }
    }
    var l = List[BeanParamDecl]()
    for ((name, (g, s)) ← map; if (g != null && s != null)) {
      l ::= new BeanParamDecl(g, s, scope.getJavaType(g.returnType))
    }
    l.sortBy(_.name.str)
  }
  def loadClass(cl: ClassLoader) =
    try { Some(cl.loadClass(fqName.str)) }
    catch { case e: Exception ⇒ println("types.scala: " + e); None }
  def properties(controller: Controller, valDef: ValDef): List[ParamProperty] = {
    for (
      b ← beanProperties;
      val t = Values.typeFor(b);
      if (!t.isInstanceOf[InvalidValueType])
    ) yield new BeanProperty(controller, valDef, b)
  }

}
// only for compilation
class BoxSymbol(
  val image: Option[String],
  var isVisual: Boolean,
  binding: ReferenceBinding,
  scope: ZaluumClassScope)
    extends ClassJavaType(binding, scope)
    with TemplateSymbol with Namer with DeclSymbol[BoxDef] {

  var initMethod: Option[MethodBinding] = None
  var hasApply = false
  var constructors = List[ConstructorDecl]()
  var methodSelector: Name = _
  var source: Option[String] = None
  override def templateTree = decl.template
  def onlyVisual = !hasApply && isVisual
  def usedNames = usedValNames ++ (ports.keySet map { _.str })
  def usedValNames = (block :: block.deepBlocks).flatMap { _.usedValNames }.toSet
  def block = blocks.head
  def argsInOrder = ports.values.toList filter { p ⇒ p.dir == In } sortBy { _.name.str }
  def returnPort = ports.values.toList find { p ⇒ p.dir == Out && !p.isField }
  def fieldReturns = ports.values.toList filter { p ⇒ p.isField && p.dir == Out } sortBy { _.name.str }
  def returnDescriptor = returnPort flatMap { _.tpe } map { _.fqName.descriptor } getOrElse ("V")
  def methodSignature = "(" + argsInOrder.flatMap { _.tpe }.map { _.fqName.descriptor }.mkString + ")" + returnDescriptor
  def mainBS = this
}

