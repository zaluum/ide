package org.zaluum.ide
import org.eclipse.draw2d._
import org.eclipse.gef._
import parts._
import org.eclipse.swt.SWT

abstract class HighlightEditPolicy[T] extends org.eclipse.gef.editpolicies.GraphicalEditPolicy{
  private var opacity : Int = _
  
  override def eraseTargetFeedback(request: Request){
    if (opacity != 0){
      containerBackground = opacity
      opacity = 0;
    }
  }
  
  private def containerBackground :java.lang.Integer= containerFigure.getAlpha
  private def containerBackground_= (alpha: Int) { containerFigure.setAlpha(alpha)}
  protected def containerFigure : Shape 
  
  override def getTargetEditPart(request : Request) : EditPart = { 
      if (request.getType() == RequestConstants.REQ_SELECTION_HOVER)
       getHost else null
  }
  protected def figure = getHost().asInstanceOf[GraphicalEditPart].getFigure().asInstanceOf[T]
  protected def showHighlight() {
    if (opacity == 0){
      opacity = if (containerBackground!=null) containerBackground.intValue else 100;
      containerBackground = 100;
    }
  }
  import RequestConstants._
  override def showTargetFeedback(request:Request) {
    val r = request.getType
    if (r==REQ_MOVE || r==REQ_ADD || r==REQ_CLONE || r==REQ_CONNECTION_START 
        || r==REQ_CONNECTION_END || r==REQ_CREATE || r==REQ_SELECTION)
      showHighlight()
  }
}
