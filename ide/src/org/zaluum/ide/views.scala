package org.zaluum.ide

import org.eclipse.gef.ui.parts.ContentOutlinePage
import org.eclipse.gef.tools.SelectEditPartTracker
import org.eclipse.swt.widgets.Composite
import org.eclipse.gef.EditPartFactory
import org.eclipse.gef.EditPart
import org.eclipse.gef.editparts.AbstractTreeEditPart
import org.zaluum.runtime.Observer
import org.zaluum.runtime.Subject
import com.google.common.collect.Lists
import java.util.ArrayList
import scala.collection.JavaConversions._
import org.eclipse.gef.Request
import org.eclipse.gef.RequestConstants
import org.eclipse.gef.ui.parts.TreeViewer
import org.zaluum.runtime.PersistentModel._

class ZaluumOutlinePage(boxEditor : ZFileEditor) extends ContentOutlinePage(new TreeViewer()) with Observer {
  override def createControl(parent : Composite) = {
    super.createControl(parent)
    getViewer.setEditDomain(boxEditor.editDomain)
    getViewer.setEditPartFactory(new ZaluumOutlineFactory(boxEditor.model))
    getViewer.setContents(boxEditor.model.root)
    boxEditor.model.root.addObserver(this)
  }
  override def dispose = boxEditor.model.removeObserver(this); super.dispose()
  override def receiveUpdate(subject:Subject) = getViewer.getContents.refresh
}

class ZaluumOutlineFactory(model : PersistentEditParts.PModel) extends EditPartFactory {
  var manager = model
  class ListenerAbstractTreePart(model : Object) extends AbstractTreeEditPart with Observer {
    setModel(model)
    def getObservable = { getModel.asInstanceOf[Subject] }
    override def activate = {
      if (!isActive) {
        super.activate
        manager.addObserver(this)
        getObservable.addObserver(this)
      }
    }
    override def deactivate = {
      if (isActive) {
        super.deactivate
        getObservable.removeObserver(this)
        manager.removeObserver(this)
      }
    }
    override def receiveUpdate(model : Subject) = refresh
  }
  override def createEditPart(c:EditPart, m:Object) =  m match {
    case c : ComposedPBox=> 
      new ListenerAbstractTreePart(m) {
        override def getModelChildren = new ArrayList(c.boxes)
        override def getText = c.name;
        override def getImage = Activator.getDefault.getImageRegistry.get("composed_16")
        override def getDragTracker(req : Request) = new SelectEditPartTracker(this)
        override def performRequest(req : Request) = req.getType match {
          case RequestConstants.REQ_OPEN => if(manager!=null) { /*TODO: Open Selected Part*/ }
          case _ => super.performRequest(req)
        }
      }
    case b: PBox => 
     new ListenerAbstractTreePart(m) {
        override def getText = b.name
        override def getImage = Activator.getDefault.getImageRegistry.get("composed_16")
        override def getDragTracker(req : Request) = new SelectEditPartTracker(this)
      }
    case _ => error("unexpected box type " + m)
  }
}




import java.awt.Font;
import org.eclipse.swt.SWT;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.ui.part.ViewPart;
import org.jfree.chart.ChartFactory;
import org.jfree.chart.JFreeChart;
import org.jfree.chart.plot.PiePlot;
import org.jfree.data.general.DefaultPieDataset;
import org.jfree.data.xy.XYSeries;
import org.jfree.data.general.PieDataset;
import org.jfree.experimental.chart.swt.ChartComposite;
import java.awt.Color;
import org.jfree.chart.ChartFactory;
import org.jfree.chart.ChartPanel;
import org.jfree.chart.JFreeChart;
import org.jfree.chart.axis.NumberAxis;
import org.jfree.chart.plot.PlotOrientation;
import org.jfree.chart.plot.XYPlot;
import org.jfree.chart.renderer.xy.XYLineAndShapeRenderer;
import org.jfree.data.xy.XYDataset;
import org.jfree.data.xy.XYSeries;
import org.jfree.data.xy.XYSeriesCollection;
import org.jfree.ui.ApplicationFrame;
import org.jfree.ui.RefineryUtilities;

class PlotView extends ViewPart {
	
	def createPartControl(parent : Composite ) = new ChartComposite(parent, SWT.NONE, createChart(createDataset),true)
  def setFocus {}
  def createDataset = {
    var series2 = new XYSeries("Second");
    series2.add(1.0, 5.0);
    series2.add(2.0, 7.0);
    series2.add(3.0, 6.0);
    series2.add(4.0, 8.0);
    series2.add(5.0, 4.0);
    series2.add(6.0, 4.0);
    series2.add(7.0, 2.0);
    series2.add(8.0, 1.0);
    val dataset = new XYSeriesCollection
    dataset.addSeries(series2)
    dataset
  }
  def createChart(dataset : XYDataset) = {
  	var chart = ChartFactory.createXYLineChart("Plotting","X","Y",dataset,PlotOrientation.VERTICAL,true,true,false)
    chart.setBackgroundPaint(Color.white)
    val plot = chart.getXYPlot()
    plot.setBackgroundPaint(Color.lightGray)
    plot.setDomainGridlinePaint(Color.white)
    plot.setRangeGridlinePaint(Color.white)
    val renderer = new XYLineAndShapeRenderer()
    renderer.setSeriesLinesVisible(0, false)
    renderer.setSeriesShapesVisible(1, false)
    plot.setRenderer(renderer)
    val rangeAxis = plot.getRangeAxis.asInstanceOf[NumberAxis]
    rangeAxis.setStandardTickUnits(NumberAxis.createIntegerTickUnits());
  	chart
  }
}









