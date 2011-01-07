package org.zaluum.nide
import org.eclipse.draw2d._
import org.eclipse.draw2d.geometry._
import org.eclipse.ui.PlatformUI
import org.eclipse.swt.SWT
import org.eclipse.gef.handles.HandleBounds
import org.eclipse.gef.util._
import org.eclipse.jface.resource.FontRegistry;
import org.eclipse.swt.graphics.Font;
import org.eclipse.ui.PlatformUI;
import org.eclipse.ui.themes.IThemeManager;
import java.util.ArrayList
object FiguresConstants {
  val MIN_DIMENSION = new Dimension(48,48)
  val HEADER_HEIGHT = 24 
  val SPACING = 5;
}
import FiguresConstants._
object Conversions {
  implicit def SRectangleToRectangle (r:SRectangle) = new Rectangle(r.pos._1,r.pos._2, r.size._1 ,r.size._2)
  implicit def RectangleToSRectangle (r:Rectangle) = SRectangle((r.x,r.y),(r.width,r.height))
}
import scala.collection.JavaConversions._
import Conversions._ 

trait PlotGraphic {
	
}

class BoxFigure extends Figure with HandleBounds with ZaluumConstraint{
  setLayoutManager(new PortsLayout())
  
  val boxFont = {   
    val themeManager = PlatformUI.getWorkbench().getThemeManager();
    val fontRegistry = themeManager.getCurrentTheme().getFontRegistry();
    fontRegistry.get("org.zaluum.ide.fontDefinition");
  }
  val nameL : BoxLabel = new BoxLabel("label")
    nameL.setTextAlignment(SWT.CENTER)
    nameL.setForegroundColor(ColorConstants.darkBlue)
    nameL.setFont(boxFont)
    add(nameL)
    
  val numChilds = new BoxLabel("")
  	numChilds.setFont(boxFont)
  	numChilds.setForegroundColor(ColorConstants.lightBlue)
  	add(numChilds)
  	
  val rectangle = new RoundedRectangle
  	rectangle.setOpaque(false);
  	rectangle.setAlpha(50);
  	rectangle.setBackgroundColor(ColorConstants.lightBlue);
    add(rectangle)
    
  def numChild = { numChilds.getText }
  def numChild_= (num:Int) {numChilds.setText(if(num==0) "" else num.toString) }
  def name = { nameL.getText }
  def name_= (nname:String) { nameL.setText(nname) }
  override def getHandleBounds = getConstraint
  override def getConstraint = rectangle.getBounds
  def slotFromPosition(position :Point) = {
    var y = (position.y - HEADER_HEIGHT);
    if (y < 0)
      y = 1;
    else if (y > getBounds().height)
      y = getBounds().height - HEADER_HEIGHT;
    y = y / 12;
    if (position.x < getBounds().width / 2)
      (y, true);
    else
      (y, false);
  }
  override def findFigureAt(x : Int, y : Int, search : TreeSearch) : IFigure = {
    if (!containsPoint(x, y) || search.prune(this)) return null
    val child = findDescendantAtExcluding(x, y, new TreeSearch() {
      override def prune(figure : IFigure) = if (figure==rectangle) true else search.prune(figure)
      override def accept(f:IFigure) = search.accept(f)
    })
    if (child != null) return child
    else if (search.accept(this)) return this
    return null
  }
}

class PortsLayout extends XYLayout {

  override def layout(container : IFigure) {
    // If the figure doesn't has a BaseBoxFigure child, this manager cannot
    // layout
    assert(container.isInstanceOf[BoxFigure])
    val b = container.asInstanceOf[BoxFigure]
    // Getting anchors and checking the size of layout
    val leftAnchors = new ArrayList[PortFigure]()
    val rightAnchors = new ArrayList[PortFigure]()
    var maxLeft, maxRight = 0;

    for (o <- container.getChildren()) o match {
      case a : PortFigure =>         
        var dist = 0;
        if (a.left) {
          dist = a.neededWidth
          leftAnchors.add(a)
          if (dist > maxLeft)
            maxLeft = dist
        } else {
          dist = a.neededWidth
          rightAnchors.add(a)
          if (dist > maxRight)
            maxRight = dist
        }
      case _ =>
    }
      
    // Updating needed bounds and placing base figure
    val bounds = b.getBounds.getCopy;
    val tl = bounds.getTopLeft();
    b.setBounds(new Rectangle(
        tl.x - maxLeft, 
        tl.y, 
        bounds.width + maxLeft + maxRight, 
        bounds.height));
    setBoundsOfChild(b, b.rectangle , new Rectangle(
        maxLeft, 
        0,
        bounds.width, 
        bounds.height));
    setBoundsOfChild(b,b.nameL, SRectangle((maxLeft,0),(bounds.width,20)))
    setBoundsOfChild(b,b.numChilds, SRectangle((0,0),(20,20)))

    // Placing anchors
    for (child <- leftAnchors) {
      setBoundsOfChild(b, child, new Rectangle(new Point(maxLeft
          - child.neededWidth, child.slotPosition), child
          .getPreferredSize()));
      
    }
    for (child <- rightAnchors) 
      setBoundsOfChild(b, child, new Rectangle(new Point(
          bounds.width + maxLeft - child.textWidth
              - SPACING, child.slotPosition),
          child.getPreferredSize()));
  }

  def setBoundsOfChild(parent : IFigure, child: IFigure, bounds : Rectangle) {
    parent.getClientArea(Rectangle.SINGLETON);
    bounds.translate(Rectangle.SINGLETON.x, Rectangle.SINGLETON.y);
    child.setBounds(bounds);
  }

}
case class SRectangle(pos:(Int,Int), size:(Int,Int))

class BoxLabel(name:String) extends Label(name){
  override protected def getTruncationString() = "..."
}

trait DirectEditFigure {

  def getTextToEdit :String 
  def getFont : Font
  def getEditLabel : BoxLabel 
  
}
class PortFigure extends Figure with DirectEditFigure {
  var left = false
  private var slot = 0
  val mainLayout = new ToolbarLayout()
    mainLayout.setVertical(false)
    mainLayout.setSpacing(SPACING)
    setLayoutManager(mainLayout)
  val anchorFont = {
    val themeManager = PlatformUI.getWorkbench().getThemeManager()
    val fontRegistry = themeManager.getCurrentTheme().getFontRegistry()
    fontRegistry.get("org.zaluum.ide.fontDefinition")
    }
  val triangle = { 
    val t = new Triangle()
    t.setDirection(SWT.RIGHT)
    t.setSize(SPACING, SPACING)
    t.setAlpha(60)
    t
    }
  private def newLabel(s:String) = {    
    val b = new BoxLabel(s);
    b.setFont(anchorFont);
    b.setForegroundColor(ColorConstants.darkGray);
    b
  }
  val name = newLabel("name") 
  val link  = newLabel("link")
  val value = newLabel("")
  val anchor = new PortConnectionAnchor(this);
  
  def linkWidth = link.getPreferredSize().width
  def textWidth = name.getPreferredSize().width
  def valueWidth = value.getPreferredSize().width
  def triangleWidth = triangle.getPreferredSize().width + SPACING
  def slotPosition = 24 - SPACING + slot*12 - 1;
  override def useLocalCoordinates : Boolean = true
  def neededWidth = valueWidth + linkWidth + triangleWidth
  override def getFont = anchorFont
  override def getTextToEdit = link.getText
  override def getEditLabel = link
  def arrange(in:Boolean, left:Boolean, slot:Int, nname:String, label:String, v:String ="") {
    this.left = left
    this.slot = slot
    triangle.setDirection(
        if (!(in ^ left)) 
          PositionConstants.EAST 
        else 
          PositionConstants.WEST)
    if (left) {
      add(link); add(value); add(triangle);  add(name);
    } else {
      add(name);  add(triangle);  add(value); add(link);
    }
    name.setText(nname)
    link.setText(label)
    value.setText(v)
  }
  class PortConnectionAnchor(owner:IFigure) extends AbstractConnectionAnchor(owner){

    override def getLocation(reference : Point) = {
      var location = triangle.getLocation();
      location.y+=SPACING+1;
      if(!left)
        location.x+=SPACING;
      triangle.translateToAbsolute(location);
      location;
    }
  }
}
object WireFigure {
  def apply() = {
    val conn = new PolylineConnection();
    val arrow = new PolygonDecoration();
    arrow.setFill(true);
    arrow.setBackgroundColor(ColorConstants.lightGray);
    arrow.setAntialias(SWT.ON);
    conn.setTargetDecoration(arrow);
    conn.addRoutingListener(RoutingAnimator.getDefault());
    conn.setAntialias(SWT.ON);
    conn.setLineWidth(1);
    conn.setForegroundColor(ColorConstants.darkGray);
    conn
  }
}