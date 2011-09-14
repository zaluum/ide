package org.zaluum.nide.compiler
import org.eclipse.swt.graphics.RGB
import org.eclipse.draw2d.ColorConstants
import org.eclipse.swt.graphics.FontData
import org.eclipse.ui.views.properties.TextPropertyDescriptor
import org.eclipse.ui.views.properties.ColorPropertyDescriptor
import org.eclipse.ui.views.properties.PropertyDescriptor
import org.eclipse.swt.widgets.Composite
import org.eclipse.jface.viewers.DialogCellEditor
import org.eclipse.swt.widgets.FontDialog
import org.eclipse.jface.viewers.LabelProvider
import org.eclipse.swt.widgets.Control
import org.eclipse.swt.SWT
import org.eclipse.ui.views.properties.ComboBoxPropertyDescriptor
import javax.swing.SwingConstants
import org.zaluum.nide.eclipse.integration.model.ZaluumClassScope
import java.awt.Rectangle
import java.awt.Toolkit
import java.awt.image.BufferedImage
import javax.imageio.ImageIO
import java.net.URL
import org.zaluum.nide.icons.Icons
import java.awt.Image
import org.zaluum.widget.ImageReader
import org.zaluum.nide.eclipse.ZaluumProject
trait Value {
  def encoded: String
  def codeGen: Tree
  def valid: Boolean = try { parse; true } catch { case e ⇒ false }
  def parse: Any
  def toSWT: AnyRef = encoded
  def valueTpe: ValueType
}
trait ZaluumParseValue {
  def parse(zp: ZaluumProject): Any
}
trait ValueType {
  def tpe: Name
  def matches(b: BeanParamSymbol): Boolean
  def matchesTpe(tpe: Name): Boolean
  def create(str: String): Value
  def defaultSWT: AnyRef = ""
  def parseSWT(a: AnyRef): String
  def editor(b: BeanParamSymbol): PropertyDescriptor = new TextPropertyDescriptor(b, b.name.str)
}
abstract class PrimitiveValueType(val ptpe: PrimitiveJavaType) extends ValueType {
  def tpe = ptpe.fqName
  def matches(b: BeanParamSymbol) = b.tpe == ptpe
  def matchesTpe(tpe: Name) = tpe == ptpe.fqName
  def parseSWT(a: AnyRef) = a.toString
}
abstract class ClassValueType(clazz: Class[_]) extends ValueType {
  val tpe = Name(clazz.getName())
  def matches(b: BeanParamSymbol) = b.tpe.fqName == tpe
  def matchesTpe(otpe: Name) = tpe == otpe
  def parseSWT(a: AnyRef) = a.toString
}
abstract class PrimitiveValue(val encoded: String, val valueTpe: ValueType) extends Value {
  def codeGen = Const(parse, valueTpe.tpe)
}
object BooleanValueType extends PrimitiveValueType(primitives.Boolean) {
  def create(str: String) = new PrimitiveValue(str, BooleanValueType) {
    def parse = java.lang.Boolean.valueOf(encoded).booleanValue
  }
}
object CharValueType extends PrimitiveValueType(primitives.Char) {
  def create(str: String) = new PrimitiveValue(str, CharValueType) {
    def parse = encoded(0)
  }
}
object ByteValueType extends PrimitiveValueType(primitives.Byte) {
  def create(str: String) = new PrimitiveValue(str, ByteValueType) {
    def parse = java.lang.Byte.valueOf(encoded).byteValue()
  }
}
object ShortValueType extends PrimitiveValueType(primitives.Short) {
  def create(str: String) = new PrimitiveValue(str, ShortValueType) {
    def parse = java.lang.Short.valueOf(encoded).shortValue()
  }
}
object IntValueType extends PrimitiveValueType(primitives.Int) {
  def create(str: String) = new PrimitiveValue(str, IntValueType) {
    def parse = Integer.decode(encoded).intValue()
  }
}
object LongValueType extends PrimitiveValueType(primitives.Long) {
  def create(str: String) = new PrimitiveValue(str, LongValueType) {
    def parse = java.lang.Long.valueOf(encoded).longValue()
  }
}
object FloatValueType extends PrimitiveValueType(primitives.Float) {
  def create(str: String) = new PrimitiveValue(str, FloatValueType) {
    def parse = java.lang.Float.valueOf(encoded).floatValue()
  }
}
object DoubleValueType extends PrimitiveValueType(primitives.Double) {
  def create(str: String) = new PrimitiveValue(str, DoubleValueType) {
    def parse = java.lang.Double.valueOf(encoded).doubleValue()
  }
}
object StringValueType extends ClassValueType(classOf[String]) {
  def create(str: String) = new Value {
    val encoded = str
    val valueTpe = StringValueType
    def parse = str
    def codeGen = Const(parse, tpe)
  }
}
object DimensionValueType extends ClassValueType(classOf[java.awt.Dimension]) {
  def create(str: String) = new Value {
    val encoded = str
    val valueTpe = DimensionValueType
    def parse: Dimension = {
      val splitted = encoded.split(" ")
      if (splitted.length == 2)
        new Dimension(Integer.decode(splitted(0)), Integer.decode(splitted(1)))
      else throw new Exception
    }
    lazy val parsed = parse
    def codeGen =
      New(tpe, List(
        new Const(parsed.w, primitives.Int),
        new Const(parsed.h, primitives.Int)), "(II)V")
  }
}
object RectangleValueType extends ClassValueType(classOf[Rectangle]) {
  def create(str: String) = new Value {
    val encoded = str
    val valueTpe = RectangleValueType
    def parse: Rectangle = {
      val splitted = encoded.split(" ")
      if (splitted.length == 4)
        new Rectangle(
          Integer.decode(splitted(0)),
          Integer.decode(splitted(1)),
          Integer.decode(splitted(2)),
          Integer.decode(splitted(3)))
      else throw new Exception
    }
    lazy val parsed = parse
    def codeGen =
      New(tpe, List(
        new Const(parsed.x, primitives.Int),
        new Const(parsed.y, primitives.Int),
        new Const(parsed.width, primitives.Int),
        new Const(parsed.height, primitives.Int)), "(IIII)V")
    override def toSWT = parsed.x + " " + parsed.y + " " + parsed.width + " " + parsed.height
  }
}
object ColorValueType extends ClassValueType(classOf[java.awt.Color]) {
  def create(str: String) = new Value {
    val encoded = str
    def valueTpe = ColorValueType
    def parse: java.awt.Color = {
      val sp = encoded.split(" ")
      if (sp.size == 3) new java.awt.Color(sp(0).toInt, sp(1).toInt, sp(2).toInt)
      else throw new Exception()
    }
    lazy val parsed = parse
    override def toSWT = if (!valid) null else
      new RGB(parsed.getRed, parsed.getGreen, parsed.getBlue)
    def codeGen =
      New(tpe, List(
        new Const(parsed.getRed(), primitives.Int),
        new Const(parsed.getGreen(), primitives.Int),
        new Const(parsed.getBlue(), primitives.Int),
        new Const(parsed.getAlpha(), primitives.Int)), "(IIII)V")
  }
  override def parseSWT(a: AnyRef) = a match {
    case rgb: RGB ⇒ rgb.red + " " + rgb.green + " " + rgb.blue
    case _        ⇒ null
  }
  override def defaultSWT = null
  override def editor(b: BeanParamSymbol) =
    new ColorPropertyDescriptor(b, b.name.str)
}
object FontValueType extends ClassValueType(classOf[java.awt.Font]) {
  def create(str: String) = new Value {
    val encoded = str
    def parse = java.awt.Font.decode(encoded)
    lazy val parsed = parse
    def codeGen =
      InvokeStatic("decode", List(Const(encoded, Name("java.lang.String"))), tpe, "(Ljava/lang/String;)Ljava/awt/Font;")
    override def toSWT = {
      if (!valid) null else {
        var flags = 0
        if (parsed.isBold) flags |= SWT.BOLD
        if (parsed.isItalic) flags |= SWT.ITALIC
        if (parsed.isPlain()) flags |= SWT.NORMAL
        new FontData(parsed.getFontName, parsed.getSize, flags)
      }
    }
    def valueTpe = FontValueType
  }
  override def parseSWT(a: AnyRef): String = a match {
    case font: FontData ⇒ fontToSwingStr(font)
    case _              ⇒ ""
  }
  override def defaultSWT = null
  override def editor(b: BeanParamSymbol) = new FontDataPropertyDescriptor(b, b.name.str)
  def fontToSwingStr(font: FontData) = {
    val flags = if ((font.getStyle & SWT.BOLD) != 0) {
      if ((font.getStyle & SWT.ITALIC) != 0) "bolditalic" else "bold"
    } else {
      if ((font.getStyle & SWT.ITALIC) != 0) "italic" else "plain";
    }
    font.name + "-" + flags + "-" + font.getHeight
  }
}
object ImageValueType extends ClassValueType(classOf[java.awt.Image]) {
  def create(str: String) = new Value with ZaluumParseValue {
    val encoded = str
    def parse(zp: ZaluumProject) = try {
      ImageIO.read(zp.classLoader.getResource(encoded))
    } catch { case e ⇒ null }
    override def valid = true
    def parse = throw new UnsupportedOperationException
    def codeGen =
      InvokeStatic(
        "readImageResource",
        List(Const(encoded, Name("java.lang.String"))),
        Name(classOf[ImageReader].getName),
        "(Ljava/lang/String;)Ljava/awt/Image;")
    def valueTpe = ImageValueType
  }
}
class IntEnumValueType(pack: String, property: String, list: List[(Int, String)]) extends PrimitiveValueType(primitives.Int) {
  def create(str: String): Value = new PrimitiveValue(str, IntEnumValueType.this) {
    def parse = Integer.decode(encoded).intValue()
    override def toSWT = (list.indexWhere(_._1 == parse) + 1).asInstanceOf[AnyRef]
  }
  override def parseSWT(a: AnyRef) = a match {
    case i: Integer ⇒ if (i == 0) "" else {
      list.lift(i - 1).map { _._1.toString }.getOrElse("0")
    }
  }
  override def matches(b: BeanParamSymbol) = {
    b.declaringClass.startsWith(pack) && property == b.name.str && b.tpe == primitives.Int
  }
  override def defaultSWT = 0.asInstanceOf[AnyRef]
  override def editor(b: BeanParamSymbol) =
    new ComboBoxPropertyDescriptor(b, b.name.str, ("" :: list.map { _._2 }).toArray)
}
class InvalidValueType(val tpe: Name) extends ValueType {
  def create(str: String): Value = new Value {
    val encoded = str
    override def parse = throw new Exception
    override def valid = false
    override def codeGen = throw new Exception
    def valueTpe = InvalidValueType.this
  }
  def parseSWT(a: AnyRef) = a.toString
  def matchesTpe(name: Name) = tpe == name
  def matches(b: BeanParamSymbol) = matchesTpe(b.tpe.fqName)
}
object Values {
  val AbstractButton = classOf[javax.swing.AbstractButton].getName
  val Colorc = classOf[java.awt.Color].getName
  val Dimensionc = classOf[java.awt.Dimension].getName

  val Fontc = classOf[java.awt.Font].getName
  val Stringc = classOf[String].getName

  val lcrlt = List(
    (SwingConstants.LEFT, "LEFT"),
    (SwingConstants.CENTER, "CENTER"),
    (SwingConstants.RIGHT, "RIGHT"),
    (SwingConstants.LEADING, "LEADING"),
    (SwingConstants.TRAILING, "TRAILING"))
  val lcr = List(
    (SwingConstants.LEFT, "LEFT"),
    (SwingConstants.CENTER, "CENTER"),
    (SwingConstants.RIGHT, "RIGHT"))
  val tcp = List(
    (SwingConstants.TOP, "TOP"),
    (SwingConstants.CENTER, "CENTER"),
    (SwingConstants.BOTTOM, "BOTTOM"))
  var types = List[ValueType]()
  private def add(pack: String, property: String, tpe: Name, list: List[(Int, String)]) {
    types ::= new IntEnumValueType(pack, property, list)
  }
  val Swing = "javax.swing"
  add(Swing, "horizontalAlignment", primitives.Int.fqName, lcrlt)
  add(Swing, "horizontalTextPosition", primitives.Int.fqName, lcrlt)
  add(Swing, "verticalTextPosition", primitives.Int.fqName, tcp)
  add(Swing, "verticalAlignment", primitives.Int.fqName, tcp)
  types ++= List(BooleanValueType,
    ByteValueType,
    ColorValueType,
    CharValueType,
    DimensionValueType,
    DoubleValueType,
    FloatValueType,
    FontValueType,
    IntValueType,
    ImageValueType,
    LongValueType,
    RectangleValueType,
    ShortValueType,
    StringValueType)
  def typeFor(tpe: Name): ValueType = {
    types.find(_.matchesTpe(tpe)).getOrElse(new InvalidValueType(tpe))
  }
  def typeFor(b: BeanParamSymbol): ValueType = {
    types.find(_.matches(b)).getOrElse(new InvalidValueType(b.tpe.fqName))
  }
  def parseNarrowestLiteral(v: String, zaluumScope: ZaluumClassScope): Value = {
      def narrowestInt(i: Int, s: String): Value = {
        if (i <= Byte.MaxValue && i >= Byte.MinValue) ByteValueType.create(s)
        else if (i <= Short.MaxValue && i >= Short.MinValue) ShortValueType.create(s)
        else IntValueType.create(s)
      }
      def parseIntOpt = try { Some(v.toInt) } catch { case e ⇒ None }
      def parseDoubleOpt = try { Some(v.toDouble) } catch { case e ⇒ None }
    if (v.toLowerCase == "true") BooleanValueType.create(v)
    else if (v.toLowerCase == "false") BooleanValueType.create(v)
    else if (v.endsWith("f") || v.endsWith("F")) {
      FloatValueType.create(v)
    } else if (v.endsWith("l") || v.endsWith("L")) {
      LongValueType.create(v)
    } else if (v.startsWith("\"") && v.endsWith("\"")) {
      StringValueType.create(v.substring(1, v.length - 1))
    } else {
      parseIntOpt match { // char?
        case Some(i) ⇒ narrowestInt(i, v)
        case None ⇒
          parseDoubleOpt match {
            case Some(d) ⇒ DoubleValueType.create(v)
            case None    ⇒ StringValueType.create(v)
          }
      }
    }

  }

}
class FontDataPropertyDescriptor(id: AnyRef, displayName: String) extends PropertyDescriptor(id, displayName) {
  setLabelProvider(new LabelProvider() {
    override def getText(element: AnyRef) = element match {
      case f: FontData ⇒ FontValueType.fontToSwingStr(f)
      case _           ⇒ ""
    }
  })
  override protected def createPropertyEditor(parent: Composite) = {
      def validator = getValidator
    new DialogCellEditor(parent) {
      setValidator(validator)
      override protected def openDialogBox(cell: Control) = {
        val dialog = new FontDialog(cell.getShell)
        val v = getValue
        v match {
          case fd: FontData ⇒ dialog.setFontList(Array(fd))
          case _            ⇒
        }
        dialog.open() match {
          case fd: FontData ⇒ fd
          case _            ⇒ v
        }
      }
    }
  }
}