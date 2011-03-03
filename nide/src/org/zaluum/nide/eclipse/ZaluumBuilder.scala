package org.zaluum.nide.eclipse;

import org.zaluum.nide.compiler.Reporter
import org.zaluum.nide.protobuf.BoxFileProtos
import org.zaluum.nide.compiler._
import java.io.ByteArrayInputStream
import java.util.{ Map ⇒ JMap }
import org.eclipse.core.resources.{ IncrementalProjectBuilder, IResourceDeltaVisitor, IResourceDelta, IResource, IProject, IMarker, IFile, IContainer }
import org.eclipse.core.runtime.{ IProgressMonitor, Path, IPath }
import org.eclipse.jdt.internal.core.JavaModelManager

object ZaluumBuilder {
  val BUILDER_ID = "org.zaluum.nide.zaluumBuilder";
  val MARKER_TYPE = "org.zaluum.nide.zaluumProblem";
}
class ZaluumBuilder extends IncrementalProjectBuilder with EclipseUtils {

  class SampleDeltaVisitor extends IResourceDeltaVisitor {
    def visit(delta: IResourceDelta) = {
      val resource = delta.getResource
      delta.getKind match {
        case IResourceDelta.ADDED ⇒
        // handle added resource
        case IResourceDelta.REMOVED ⇒
        // handle removed resource
        case IResourceDelta.CHANGED ⇒
        // handle changed resource
        case _ ⇒
      }
      //return true to continue visiting children.
      true;
    }
  }

  private def addMarker(file: IFile, message: String, blame: Option[Location],
    severity: Int) {
    val marker = file.createMarker(ZaluumBuilder.MARKER_TYPE);
    marker.setAttribute(IMarker.MESSAGE, message + blame.toString);
    marker.setAttribute(IMarker.SEVERITY, severity);
    marker.setAttribute("BLAME", blame.map { _.toString } getOrElse (Location(List()).toString));
  }

  protected def build(kind: Int, args: JMap[_, _], monitor: IProgressMonitor): Array[IProject] = {
    /*if (kind == IncrementalProjectBuilder.FULL_BUILD) {
      fullBuild(monitor);
    } else {
      val delta = getDelta(getProject());
      if (delta == null) {
        fullBuild(monitor);
      } else {
        incrementalBuild(delta, monitor);
      }
    }*/
    fullBuild(monitor)
    return null;
  }

  private def deleteMarkers(file: IFile) {
    file.deleteMarkers(ZaluumBuilder.MARKER_TYPE, false, IResource.DEPTH_ZERO);
  }
  def jmodel = JavaModelManager.getJavaModelManager.getJavaModel
  def jproject = jmodel.getJavaProject(getProject);
  def defaultOutputFolder = { jproject.getOutputLocation }
  def root = getProject.getWorkspace.getRoot

  def compile(f: IFile, cl: EclipseBoxClasspath) = {
    val reporter = new Reporter
    try {
      cl.toClassName(f) foreach { className ⇒
        val proto = BoxFileProtos.BoxClassDef.parseFrom(f.getContents(true))
        val tree = Parser.parse(proto, Some(className))
        val scope = new FakeGlobalScope(cl)
        val analyzedTree = new Analyzer(reporter, tree, scope).compile()
        def generate(tree: Tree) {
          val sym = tree.symbol.asInstanceOf[BoxTypeSymbol]
          println("generating " + sym.fqName)
          val classTree = new TreeToClass(tree, scope).run()
          val outputPath = defaultOutputFolder.append(new Path(sym.fqName.toRelativePathClass))
          writeFile(outputPath, ByteCodeGen.dump(classTree))
          tree match {
            case b: BoxDef ⇒ b.defs foreach { case c:BoxDef ⇒ generate(c) }
          }
        }
        val bd = analyzedTree.asInstanceOf[BoxDef]        
        reporter.check()
        generate(analyzedTree)
      }
    } catch {
      case e: CompilationException ⇒
        for (err ← reporter.errors) {
          addMarker(f, err.msg, err.mark, IMarker.SEVERITY_ERROR)
        }
    }
  }
  def writeFile(path: IPath, bytes: Array[Byte]) {
    val is = new ByteArrayInputStream(bytes)
    val outputFile = root.getFile(path)
    if (outputFile.exists)
      outputFile.setContents(is, true, false, null)
    else
      outputFile.create(is, true, null)
  }
  protected def fullBuild(monitor: IProgressMonitor) {
    getProject.deleteMarkers(ZaluumBuilder.MARKER_TYPE, false, IResource.DEPTH_INFINITE);
    val cl = new EclipseBoxClasspath(getProject)
    cl.update()
    visitSourceZaluums { f ⇒ compile(f, cl) }
  }

  protected def incrementalBuild(delta: IResourceDelta,
    monitor: IProgressMonitor) {
    // the visitor does the work.
    delta.accept(new SampleDeltaVisitor());
  }
}
