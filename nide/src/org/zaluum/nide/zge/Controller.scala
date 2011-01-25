package org.zaluum.nide.zge
import org.zaluum.nide.compiler.ScannedBoxClassPath
import org.zaluum.nide.model._
import scala.collection.mutable.{ Buffer, Stack }
class Controller(val model:Model,val bcp:ScannedBoxClassPath) extends AbstractController[Model]{
  private var viewModels = Buffer[ModelView]()
  def registerView(viewer: Viewer) = {
    val viewModel = new ModelView(viewer, model, bcp)
    viewModels += viewModel
    viewModel.update()
    viewModel
  }
  def updateViewers { viewModels foreach { _.update() } }
  def abortTools() { viewModels foreach { _.viewer.tool.state.abort() } }  
}


