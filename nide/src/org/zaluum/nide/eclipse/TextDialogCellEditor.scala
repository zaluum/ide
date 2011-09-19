package org.zaluum.nide.eclipse

import org.eclipse.core.runtime.Assert
import org.eclipse.swt.SWT
import org.eclipse.swt.events.FocusAdapter
import org.eclipse.swt.events.FocusEvent
import org.eclipse.swt.events.KeyAdapter
import org.eclipse.swt.events.KeyEvent
import org.eclipse.swt.events.ModifyEvent
import org.eclipse.swt.events.ModifyListener
import org.eclipse.swt.events.MouseAdapter
import org.eclipse.swt.events.MouseEvent
import org.eclipse.swt.events.SelectionAdapter
import org.eclipse.swt.events.SelectionEvent
import org.eclipse.swt.events.TraverseEvent
import org.eclipse.swt.events.TraverseListener
import org.eclipse.swt.widgets.Composite
import org.eclipse.swt.widgets.Control
import org.eclipse.swt.widgets.Text
import org.eclipse.jface.viewers.TextCellEditor
import org.eclipse.swt.widgets.Layout
import org.eclipse.swt.graphics.Point
import org.eclipse.swt.widgets.Label
import org.eclipse.swt.widgets.Button
import org.eclipse.swt.events.FocusListener
import java.text.MessageFormat

abstract class TextDialogCellEditor(parent: Composite, style: Int) extends TextCellEditor(parent, style) {
  def this(parent: Composite) = this(parent, SWT.SINGLE)
  private var editor: Composite = _
  private var contents: Control = _
  private var button: Button = _
  private lazy val buttonFocusListener: FocusListener = new FocusListener() {
    def focusGained(e: FocusEvent) {
      // Do nothing
    }
    def focusLost(e: FocusEvent) {
      TextDialogCellEditor.this.focusLost();
    }
  }

  private class DialogCellLayout extends Layout {
    def layout(editor: Composite, force: Boolean) {
      val bounds = editor.getClientArea();
      val size = button.computeSize(SWT.DEFAULT, SWT.DEFAULT, force);
      if (contents != null) {
        contents.setBounds(0, 0, bounds.width - size.x, bounds.height);
      }
      button.setBounds(bounds.width - size.x, 0, size.x, bounds.height);
    }

    def computeSize(editor: Composite, wHint: Int, hHint: Int,
                    force: Boolean): Point = {
      if (wHint != SWT.DEFAULT && hHint != SWT.DEFAULT) {
        return new Point(wHint, hHint);
      }
      val contentsSize = contents.computeSize(SWT.DEFAULT, SWT.DEFAULT,
        force);
      val buttonSize = button.computeSize(SWT.DEFAULT, SWT.DEFAULT,
        force);
      // Just return the button width to ensure the button is not clipped
      // if the label is long.
      // The label will just use whatever extra width there is
      val result = new Point(buttonSize.x, math.max(contentsSize.y,
        buttonSize.y));
      return result;
    }
  }
  def createContents(cell: Composite) = {
    super.createControl(cell)
  }
  protected def createButton(parent: Composite): Button = {
    val result = new Button(parent, SWT.DOWN);
    result.setText("..."); //$NON-NLS-1$
    return result;
  }
  override def deactivate() {
    if (button != null && !button.isDisposed()) {
      button.removeFocusListener(buttonFocusListener);
    }
    super.deactivate();
  }
  override protected def createControl(parent: Composite): Control = {

    val font = parent.getFont();
    val bg = parent.getBackground();

    editor = new Composite(parent, getStyle());
    editor.setFont(font);
    editor.setBackground(bg);
    editor.setLayout(new DialogCellLayout());

    contents = createContents(editor);
    //updateContents(value);

    button = createButton(editor);
    button.setFont(font);

    button.addKeyListener(new KeyAdapter() {
      override def keyReleased(e: KeyEvent) {
        if (e.character == '\u001b') { // Escape
          fireCancelEditor();
        }
      }
    });

    button.addFocusListener(buttonFocusListener);

    button.addSelectionListener(new SelectionAdapter() {
      override def widgetSelected(event: SelectionEvent) {
        println("selected")
        // Remove the button's focus listener since it's guaranteed
        // to lose focus when the dialog opens
        button.removeFocusListener(buttonFocusListener);

        val newValue = openDialogBox(editor)

        // Re-add the listener once the dialog closes
        button.addFocusListener(buttonFocusListener);
        newValue match {
          case Some(n) ⇒
            val newValidState = isCorrect(n);
            if (newValidState) {
              markDirty();
              doSetValue(n);
            } else {
              // try to insert the current value into the error message.
              setErrorMessage(MessageFormat.format(getErrorMessage(),
                Array(n.toString())));
            }
            fireApplyEditorValue();
          case None ⇒
        }
      }
    });

    setValueValid(true);

    return editor;
  }
  override protected def focusLost() {
    /*if (isActivated()) {
			fireApplyEditorValue();
			deactivate();
		}*/
  }
  protected def openDialogBox(cellEditorWindow: Control): Option[String]

}