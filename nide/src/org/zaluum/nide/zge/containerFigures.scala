package org.zaluum.nide.zge

import scala.collection.JavaConversions.asScalaBuffer
import scala.collection.mutable.Buffer
import org.eclipse.draw2d.PositionConstants.EAST
import org.eclipse.draw2d.PositionConstants.NORTH
import org.eclipse.draw2d.PositionConstants.SOUTH
import org.eclipse.draw2d.PositionConstants.WEST
import org.eclipse.draw2d.geometry.Point
import org.eclipse.draw2d.geometry.Rectangle
import org.eclipse.draw2d.ColorConstants
import org.eclipse.draw2d.Figure
import org.eclipse.draw2d.Graphics
import org.eclipse.draw2d.ImageFigure
import org.eclipse.draw2d.Layer
import org.eclipse.draw2d.LayeredPane
import org.eclipse.draw2d.LineBorder
import org.eclipse.draw2d.PositionConstants
import org.eclipse.draw2d.Triangle
import org.eclipse.jface.resource.ImageDescriptor
import org.zaluum.nide.compiler.Block
import org.zaluum.nide.compiler.ConnectionDef
import org.zaluum.nide.compiler.ConnectionEnd
import org.zaluum.nide.compiler.Dimension
import org.zaluum.nide.compiler.IfExprType
import org.zaluum.nide.compiler.In
import org.zaluum.nide.compiler.Junction
import org.zaluum.nide.compiler.JunctionRef
import org.zaluum.nide.compiler.Name
import org.zaluum.nide.compiler.Out
import org.zaluum.nide.compiler.{ Point ⇒ MPoint }
import org.zaluum.nide.compiler.PortRef
import org.zaluum.nide.compiler.PortSide
import org.zaluum.nide.compiler.Shift
import org.zaluum.nide.compiler.Tree
import org.zaluum.nide.compiler.ValDef
import org.zaluum.nide.compiler.ValSymbol
import org.zaluum.nide.compiler.Vector2
import draw2dConversions._
import RichFigure._
import org.zaluum.nide.compiler.LiteralExprType
import org.zaluum.nide.compiler.Expressions
import org.zaluum.nide.utils.Timer
import org.zaluum.nide.compiler.BoxSymbol
import org.eclipse.draw2d.geometry.Translatable

trait ContainerItem extends Item {
  def viewer: ItemViewer
  def layer: Figure
  def connectionsLayer: Figure
  def pointsLayer: Figure
  def portsLayer: Figure
  def translateMineToAbsolute_![T <: Translatable](modified: T): T = {
    translateToParent(modified)
    translateToAbsolute(modified)
    modified
  }
  protected def itemAtIn(container: Figure, p: Point, debug: Boolean = false): Option[Item] =
    container.findDeepAt(point(p), 0, debug) {
      case i: Item ⇒ i
    }
  def itemAt(p: Point, debug: Boolean = false) = {
    itemAtIn(portsLayer, p, debug)
      .orElse(itemAtIn(layer, p, debug))
      .orElse(itemAtIn(connectionsLayer, p, debug))
  }
  def findLabelFigureOf(v: ValDef): Option[LabelItem] = {
    this.deepChildren collect { case l: LabelItem if (l.valDef == v) ⇒ l } headOption
  }
  def findFigureOf(v: ValDef) = {
    this.deepChildren collect { case i: ValDefItem if (i.valDef == v) ⇒ i } headOption
  }

  def shallowItems = {
    (portsLayer.getChildren.view ++
      layer.getChildren.view ++
      connectionsLayer.getChildren.view ++
      pointsLayer.getChildren.view).collect { case i: Item ⇒ i }
  }
  def highLight(b: Boolean)

  protected def createGraph: ConnectionGraph = {
    val portVertexs = portsLayer.getChildren collect {
      case port: PortFigure ⇒
        new PortVertex(port.ps, port.anchor)
    }
    val junctions = block.junctions.collect { case j: Junction ⇒ (j -> new Joint(j.p)) }.toMap
    val emptyVertexs = Buffer[EmptyVertex]()

    val edges = block.connections.map {
      case c: ConnectionDef ⇒
        def toVertex(t: Option[ConnectionEnd], start: Boolean): Vertex = {
              def pos = if (start) c.headPoint else c.lastPoint
            t match {
              case Some(JunctionRef(name)) ⇒
                junctions.view.collect { case (k, joint) if (k.name == name) ⇒ joint }.head
              case Some(p: PortRef) ⇒
                println(p + " " + p.sym)
                portVertexs.find(_.ps == p.sym).get // BUG not found
              case None ⇒
                val e = new EmptyVertex(pos)
                emptyVertexs += e
                e
            }
          }
        val bad = block.sym.connections.isBad(c)
        (c -> new Edge(toVertex(c.a, true), toVertex(c.b, false), c.points, Some(c), bad).fixEnds)
    }.toMap
    new ConnectionGraphV(
      portVertexs.toSet ++
        junctions.values ++
        emptyVertexs,
      edges.values.toSet)
  }
  val boxes = Buffer[ValDefItem]()
  val labels = Buffer[LabelItem]()
  def block: Block
  def symbol = block.sym
  def templateSym = symbol.template
  def template = templateSym match {
    case v: ValSymbol ⇒ v.decl.template.get
    case b: BoxSymbol ⇒ b.decl.template
  }
  val junctions = Buffer[PointFigure]()
  val connections = Buffer[ConnectionHolder]()
  var graph: ConnectionGraph = _
  type UpdatePF = PartialFunction[Tree, Tree]
  val idUpdate: UpdatePF = { case t: Tree ⇒ t }
  def updateContents(changes: UpdatePF) {
    updateBoxes(changes)
    updatePorts(changes)
    updateJunctions()
    graph = createGraph
    updateConnections()
  }
  def updateBoxes(changes: UpdatePF) {
    val remove = Buffer[ValDefItem]()
    for (bf ← boxes.view ++ labels) {
      changes.lift(bf.valDef) match {
        case Some(t: ValDef) ⇒
          bf match {
            case o: OpenBoxFigure ⇒ o.updateOpenBox(t, changes)
            case l: LabelItem ⇒
              t.label match {
                case Some(ld) ⇒
                  l.updateValDef(t)
                case None ⇒
                  l.destroy()
                  remove += l
              }
            case s: ValDefItem ⇒ s.updateValDef(t)
          }
        case _ ⇒
          bf.destroy()
          remove += bf
      }
    }
    boxes.filterNot(remove.contains)
    labels.filterNot(remove.contains)
    val news = block.valDefs filter (v ⇒ !remove.contains(v))
    news foreach { v ⇒
      val f = v.template match {
        case Some(t) ⇒
          val o = v.sym.tpe match {
            case Some(IfExprType) ⇒ new IfOpenBoxFigure(ContainerItem.this, viewer)
            case _                ⇒ new OpenBoxFigure(ContainerItem.this, viewer)
          }
          o.updateOpenBox(v, Map())
          o
        case None ⇒
          val valf = v.sym.tpe.map(_.fqName) match {
            case Some(LiteralExprType.fqName) ⇒
              new LiteralFigure(ContainerItem.this)
            case Some(name) if (Expressions.thisFigureExpressions.contains(name)) ⇒
              new ThisOpValFigure(ContainerItem.this)
            case _ ⇒
              new ImageValFigure(ContainerItem.this)
          }
          valf.updateValDef(v)
          valf
      }
      boxes += f
    }
    news foreach { v ⇒
      v.label foreach { _ ⇒
        val l = new LabelItem(ContainerItem.this)
        l.updateValDef(v)
        labels += l
      }
    }
  }
  def updatePorts(changes: UpdatePF)
  def updateJunctions() {
    junctions.foreach { this.pointsLayer.safeRemove(_) }
    junctions.clear
    for (j ← block.junctions.asInstanceOf[List[Junction]]) {
      val p = new PointFigure
      p.update(j.p, j.tpe)
      junctions += p
    }
    junctions.foreach { this.pointsLayer.add(_) }
  }
  def updateConnections() {
    connections.foreach { _.destroy() }
    connections.clear
    connections ++= graph.edges map { e ⇒ new ConnectionHolder(e, ContainerItem.this) }
  }
  override def destroy() {
    super.destroy()
    for (b ← boxes) b.destroy()
    for (l ← labels) l.destroy()
    for (c ← connections) c.destroy()
  }
}
object OpenBoxFigure {
  val backgroundNormal = ColorConstants.white
  val backgroundBlink = ColorConstants.lightGray
}
class OpenBoxFigure(
  val container: ContainerItem,
  override val viewer: ItemViewer) extends LayeredPane
    with ValDefItem
    with ResizableFeedback
    with ContainerItem
    with Transparent
    with OverlappedEffect {
  // Item
  def myLayer = container.layer
  def size = valDef.size getOrElse Dimension(50, 50)
  // layers
  lazy val layer = new Layer
  lazy val portsLayer = new Layer
  lazy val connectionsLayer = new Layer
  lazy val pointsLayer = new Layer
  setBackgroundColor(ColorConstants.white)
  // ContainerItem
  //setBackgroundColor(ColorConstants.white)
  setOpaque(true)
  def helpers = portDecls ++ portSymbols
  val portDecls = Buffer[OpenPortBaseFigure]()
  val portSymbols = Buffer[PortSymbolFigure]()
  override def useLocalCoordinates = true
  def block = valDef.sym.currentBlock.decl
  def updateOpenBox(v: ValDef, changes: UpdatePF) {
    updateValDef(v)
    updateContents(changes)
    showArrowsIfNotBigEnough
  }
  def highLight(b: Boolean) = {
    myBorder.setColor(if (b) ColorConstants.lightBlue else ColorConstants.gray)
  }
  def blink(b: Boolean) {
    setBackgroundColor(if (b) OpenBoxFigure.backgroundBlink else OpenBoxFigure.backgroundNormal)
  }
  import PositionConstants._
  private def newTriangle(pos: Int) = {
    val t = new Triangle
    t.setBackgroundColor(ColorConstants.lightGray)
    val size = (14, 80)
    val s = if (pos == EAST || pos == WEST) size else size.swap
    t.setSize(s._1, s._2)
    t.setFillXOR(true)
    t.setDirection(pos)
    t.setOrientation(if (pos == EAST || pos == WEST) PositionConstants.VERTICAL else PositionConstants.HORIZONTAL)
    t
  }
  val triangles = Map(
    EAST -> newTriangle(EAST),
    WEST -> newTriangle(WEST),
    NORTH -> newTriangle(NORTH),
    SOUTH -> newTriangle(SOUTH))

  def showArrowsIfNotBigEnough() {
    val b = new Rectangle()
    import RichFigure._
    this.deepChildren.foreach { f ⇒ b.union(f.getBounds()) }
      def showTriangle(pos: Int) = {
        val t = triangles(pos)
        if (t.getParent != layer)
          layer.add(t)
        val point = if (pos == EAST || pos == WEST) {
          val y = size.h / 2 - t.getSize.height / 2
          val x = if (pos == EAST) size.w - t.getSize.width - getInsets.left - getInsets.right
          else getInsets.left
          new Point(x, y)
        } else {
          val x = size.w / 2 - t.getSize.width / 2
          val y = if (pos == SOUTH) size.h - t.getSize.height - getInsets.top - getInsets.bottom
          else getInsets.top
          new Point(x, y)
        }
        t.setLocation(point)
      }
    triangles.values.foreach { layer.safeRemove(_) }
    if (b.width > getClientArea.getSize.width) showTriangle(EAST)
    if (b.height > getClientArea.getSize.height) showTriangle(SOUTH)
    if (b.x < 0) showTriangle(WEST)
    if (b.y < 0) showTriangle(NORTH)
  }
  def updateMe() {}
  def updateValPorts() {}
  override def destroy() {
    super.destroy()
    portDecls.foreach { _.destroy() }
    portSymbols.foreach { _.destroy() }
  }
  def updatePorts(changes: UpdatePF) {
    portDecls.foreach { _.destroy() }
    portDecls.clear()
    portSymbols.foreach(_.destroy())
    portSymbols.clear()
    val vs = valDef.sym
    vs.portSides filter { _.fromInside } foreach {
      intPs ⇒ // TODO cleanup
          def extPs = vs.portSides.find(c ⇒ c.name == intPs.name && c.inPort == intPs.inPort && !c.fromInside).get
          def newFig(left: Boolean) = {
            val f = new OpenPortDeclFigure(OpenBoxFigure.this)
            f.update(intPs, extPs, left)
            portDecls += f
          }
        if (intPs.pi.hasDecl) {
          intPs.pi.dir match {
            case In    ⇒ newFig(true)
            case Out   ⇒ newFig(false)
            case Shift ⇒ newFig(intPs.inPort);
          }
        } else {
          if (intPs.pi.dir == Out) {
            val f = new PortSymbolFigure(intPs, OpenBoxFigure.this)
            f.update
            portSymbols += f
          } else if (intPs.pi.dir == In) {
            val f = new OpenPortFixedFigure(OpenBoxFigure.this)
            f.update(intPs, extPs, true, In, MPoint(0, 10))
            portDecls += f
          }
        }
    }
  }
  override def updateSize() {
    super.updateSize()
    //inners.setSize(getBounds.getSize)
  }
  override def init() {
    super.init()
    add(layer)
    add(connectionsLayer)
    add(portsLayer)
    add(pointsLayer)
    setBorder(myBorder)
  }
  lazy val myBorder = new LineBorder(ColorConstants.gray, 6)
}
abstract class Button(val openBox: OpenBoxFigure) extends ImageFigure with OverlappedItem with RectFeedback {
  var size = Dimension(10, 10)
  def imageFactory = container.viewer.zproject.imageFactory
  def myLayer = container.layer
  def blink(b: Boolean) {}
  def extPos = MPoint(16, 0)
  def constantDisplacement = Vector2(0, -size.h + openBox.getInsets().top)
  def imageDesc: ImageDescriptor
  var currentDesc: Option[ImageDescriptor] = None
  def update() {
      def loadImage() {
        currentDesc = Some(imageDesc)
        val img = imageFactory.image(imageDesc)
        setImage(img)
        size = Dimension(img.getBounds.width, img.getBounds().height)
      }
    currentDesc match {
      case Some(c) if (c != imageDesc) ⇒
        imageFactory.destroy(c)
        loadImage()
      case None ⇒ loadImage()
      case _    ⇒
    }
    updateSize()
  }
  override def destroy() {
    super.destroy()
    currentDesc foreach { imageFactory.destroy }
    currentDesc = None
  }
}
class IfOpenBoxFigure(container: ContainerItem, viewer: ItemViewer) extends OpenBoxFigure(container, viewer) {
  lazy val buttons = Buffer[Button]()

  override def init() {
    super.init()
    buttons += new Button(this) {
      def imageDesc =
        if (openBox.symbol.blockNumeral == 0)
          imageFactory.buttonIfTrue
        else imageFactory.buttonIfFalse
    }
  }
  override def destroy() {
    super.destroy()
    buttons.foreach { _.destroy() }
  }
  override def updateMe {
    buttons.foreach { _.update() }
  }
}
