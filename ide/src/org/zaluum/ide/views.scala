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
import org.eclipse.ui.PlatformUI



import org.eclipse.ui.part.PageBookView
import org.eclipse.ui.part.PageBook
import org.eclipse.ui.part.IPage
import org.eclipse.ui.part.Page
import org.eclipse.ui.part.MessagePage
import org.eclipse.ui.IWorkbenchPage
import org.eclipse.ui.IWorkbenchPart
import org.eclipse.ui.SubActionBars
import org.eclipse.gef.util.MyPageBookView
import org.eclipse.gef.ui.views.palette.PaletteViewerPage

abstract class BaseView extends MyPageBookView {
  override protected def isImportant(part : IWorkbenchPart ) = part.isInstanceOf[BaseEditor]
	override protected def getBootstrapPart = { if (getSite.getPage != null) getSite.getPage.getActiveEditor else null }
	override protected def createDefaultPage(book : PageBook) = {
		var messagePage = new MessagePage
		messagePage.createControl(getPageBook)
		initPage(messagePage)
		messagePage
	}
}

class PlotView extends BaseView {
	override def getMyPage(part : IWorkbenchPart) = new PlotPage(part.asInstanceOf[BaseEditor])
}

class WatchView extends BaseView {
	override def getMyPage(part : IWorkbenchPart) = new WatchPage(part.asInstanceOf[BaseEditor])
}

import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Group;
import org.eclipse.swt.widgets.Table;
import org.eclipse.swt.widgets.TableColumn;
import org.eclipse.swt.widgets.TableItem;
import org.eclipse.swt.widgets.Text;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Display;
import org.eclipse.swt.layout.GridData;
import java.lang.Thread

class Sinus extends Thread with Subject {
	var t : Double = 0
	@volatile var sin : Double = 0
	override def run {
		while(true) {
			sin = scala.math.sin(t)
			t=t+1
			if(t==360) t=0
			notifyObservers
			Thread.sleep(1000)
		}
	}
}

class WatchPage(editor : BaseEditor) extends Page with Observer {
  var composite : Composite = null
  var table : Table = null
	override def getControl = composite
	override def setFocus = composite.setFocus
	override def createControl(parent : Composite) {
  	composite = new Composite(parent, SWT.FILL)
  	var layout = new GridLayout
  	layout.numColumns = 1
  	composite.setLayout(layout)
  	
  	//Values list
  	var groupValues = new Group(composite, SWT.FILL )
    var group2Layout = new GridLayout
    groupValues.setLayout(group2Layout)
    var dataValues = new GridData( SWT.FILL, SWT.FILL, false, true );
  	dataValues.widthHint = 400
    groupValues.setLayoutData(dataValues)
    groupValues.setText( " Values " )
    table = new Table( groupValues,  SWT.MULTI | SWT.BORDER | SWT.FULL_SELECTION)
    table.setLinesVisible(true)
	  table.setHeaderVisible(true)
    var data = new GridData( SWT.FILL, SWT.FILL, true, true )
    data.heightHint = 400
    table.setLayoutData(data)
    var titles = List("Name","Type","Value")
	  for (t <- titles) {
	  	var column = new TableColumn (table, SWT.NONE);
	  	column.setText(t);
	  }	
    for(c <- table.getColumns) c.pack
    
    addItem
    
    var sin = new Sinus
    sin.addObserver(this)
    sin.start
    
  }
  def addItem() {
  	var item = new TableItem (table, SWT.NONE);
    item.setText (0, "Sinus");
		item.setText (1, "Double");
		item.setText (2, "0");
  }
  override def receiveUpdate(subject:Subject) = {
    	Display.getDefault.asyncExec(new Runnable {
    		override def run = {
    			table
    			.getItem(0)
    			.setText(2,subject.asInstanceOf[Sinus]
    			.sin.
    			toString)
    		}
    	})
  }
}

class PlotPage(editor : BaseEditor) extends Page with Observer {
	var viewer : ChartComposite = null
	var composite : Composite = null
	var set : Set[Double] = Set(0)
	override def getControl = composite
	override def setFocus = composite.setFocus
	override def createControl(parent : Composite) {

		composite = new Composite(parent, SWT.FILL)
  	var layout = new GridLayout( 2, false )
  	composite.setLayout(layout)
     	
    //Plots
    var groupValuesP = new Group(composite, SWT.FILL )
    var group2LayoutP = new GridLayout
    groupValuesP.setLayout(group2LayoutP)
    var dataValuesP = new GridData( SWT.FILL, SWT.FILL, false, true );
    dataValuesP.widthHint = 400
    groupValuesP.setLayoutData(dataValuesP)
    groupValuesP.setText( " Plot " )
    viewer = new ChartComposite(groupValuesP, SWT.FILL, createChart(createDataset), true);
    var hlcDataP = new GridData( SWT.FILL, SWT.FILL, true, true )
    viewer.setLayoutData(hlcDataP)
    
    var sin = new Sinus
    sin.addObserver(this)
    sin.start
    
	}
    def createDataset() = {
    	var sinSeries = new XYSeries("Sin");
    	var seriesCollection = new XYSeriesCollection()
    	var index = 0
    	for(s <- set) {
    		sinSeries.add(index,s)
    		index = index + 1
    	}
    	seriesCollection.addSeries(sinSeries);
    	seriesCollection
    }
    
    def createChart(xydataset : XYDataset ) = {
    	var jfreechart = ChartFactory.createXYLineChart("Sin Curve Demo", "Angle (Deg)", "Y", xydataset, PlotOrientation.VERTICAL, true, true, false);
    	jfreechart.setBackgroundPaint(Color.white);
    	var xyplot = jfreechart.getPlot.asInstanceOf[XYPlot];
    	xyplot.setBackgroundPaint(Color.lightGray);
    	xyplot.setDomainGridlinePaint(Color.white);
    	xyplot.setRangeGridlinePaint(Color.white);
    	jfreechart;
    }
      override def receiveUpdate(subject:Subject) = {
    	Display.getDefault.asyncExec(new Runnable {
    		override def run = {
    			set.add(subject.asInstanceOf[Sinus].sin)
    			viewer.setChart(createChart(createDataset))
    		}
    	})
  }
}