package com.allinfinance.vrouter.simu

import scala.swing._
import scala.swing.event._
import Swing._

import javax.swing.JTree
import javax.swing.ToolTipManager
import javax.swing.JComponent
import javax.swing.JComboBox
import javax.swing.JPanel
import javax.swing.JLabel
import javax.swing.JTextField
import javax.{swing => js}
import js.{tree => jst}
import js.{event => jse}
import javax.swing.UIManager

import scalaswingcontrib.tree._
import scalaswingcontrib.event._

import com.allinfinance.xsd._
import Util._

class XsdTree extends ScrollPane {
    var reqXsd: XsdNode = null
    var respXsd: XsdNode = null

    def setFile(name: String): Boolean = {
        val respName = name.replaceAll("Request", "Response")
        try {
            reqXsd = null
            respXsd = null
            reqXsd = XsdNode.buildSchemaTree(XSParser.parse(name))
            respXsd = XsdNode.buildSchemaTree(XSParser.parse(respName))

            xsdTree.model = new ExternalTreeModel(List(reqXsd), ((n: XsdNode) => n.children)).makeUpdatableWith {
                (path, node) => 
                    path.last.value = node.value
                    path.last
            } makeInsertableWith {
                (parentPath, node, index) =>
                    //                parentPath.last.add(index, node)
                    true
            } makeRemovableWith {
                (pathToRemove) =>
                    val node = pathToRemove.last
                //                node.parent.remove(node)
                    true
            }

            xsdTree.expandAll
            true
        }
        catch {
            case e =>
                Dialog.showMessage(null, "load file[" + (if (reqXsd == null) name else respName) + "] failed!\n" + e.formatException)
                false
        }
    }

    val xsdTree = new Tree[XsdNode] {thisTree =>

        listenTo(keys)
        reactions += {
            case KeyPressed(_, Key.Enter, _, _) =>
                val sel = selection.paths.leadSelection
                sel.map{path =>
                    if (path.last.children.size == 0) // leaf
                        startEditingAtPath(path)
                }
        }

        lineStyle = Tree.LineStyle.Angled

        renderer = new Tree.DefaultRenderer[XsdNode] {
            override def componentFor(tree: Tree[_], value: XsdNode, info: companion.CellInfo): Component = {
                peer.defaultRendererComponent(tree.peer, value.asInstanceOf[AnyRef], info.isSelected, info.isExpanded, info.isLeaf, info.row, info.hasFocus)
//                backgroundNonSelectionColor = if (value.dirty) java.awt.Color.RED else java.awt.Color.WHITE
                for (annotation <- value.elmt.annotation;
                     doc <- annotation.documentation) {
                    tooltip = doc
                }
                this
            }
        }
        ToolTipManager.sharedInstance().registerComponent(peer)

        editable = true
        editor = new Tree.Editor[XsdNode] {thisEditor =>
            private[this] lazy val lazyPeer: jst.TreeCellEditor = new TreeEditorPeer {
                override def isCellEditable(e: java.util.EventObject) = {
                    if (e == null) true  // for key enter start editing
                    else {
                        val tree = getTreeWrapper(e.getSource.asInstanceOf[JTree])
//                            val sn = tree.selection.selectedNode
                        e match {
                            case me: java.awt.event.MouseEvent =>
                                val path = treePathToPath(tree.peer.getPathForLocation(me.getX, me.getY))
                                tree.model.peer.isLeaf(path.last) && path.last.isEditable
                            case _ => false
                        }
                    }
                }
            }
            override def peer = lazyPeer // We can't use a lazy val directly, as Wrapped wouldn't be able to override with a non-lazy val

            val editingIcon = UIManager.getIcon("Tree.leafIcon")
            object cellEditor extends Component {
                override lazy val peer: JComponent = new JLabel with SuperMixin {
                    val offset = 4 + editingIcon.getIconWidth
                    setLayout(null)

                    def calculateIconY: Int = {
                        val iconHeight = editingIcon.getIconHeight
                        val textHeight = lastComponent.peer.getFontMetrics(lastComponent.peer.getFont).getHeight
                        val textY = iconHeight/2 - textHeight/2
                        val totalY = math.min(0, textY)
                        val totalHeight = math.max(iconHeight, textY + textHeight) - totalY
                        getHeight/2 - (totalY + (totalHeight/2))
                    }
                    override def paint(g: java.awt.Graphics) {
                        val yLoc = calculateIconY
                        editingIcon.paintIcon(this, g, 0, yLoc)
                        super.paint(g)
                    }

                    override def getPreferredSize: Dimension = if (lastComponent == null) new Dimension(0, 0) else {
                        var sz = lastComponent.preferredSize
                        sz.width += offset + 5 + label.preferredSize.width + 2
                        sz.height = math.max(sz.height, editingIcon.getIconHeight)
                        sz.width = math.max(sz.width, 100)
                        sz
                    }

                    override def doLayout: Unit = {
                        label.peer.setBounds(offset, 0, label.preferredSize.width, getHeight)
                        lastComponent.peer.setBounds(offset+label.preferredSize.width+2, 0, getWidth - offset - label.preferredSize.width - 2, getHeight)
                    }
                }

                opaque = false
                border = null

                val label = new Label
                label.opaque = false
//              contents += label
                peer.add(label.peer)

                var lastComponent: Component = null
                reactions += {
                    case MaskEditDone(_) => thisEditor.peer.stopCellEditing
                    case EditDone(_) => thisEditor.peer.stopCellEditing                   // from textfield
                    case SelectionChanged(_) => thisEditor.peer.stopCellEditing           // from combobox
                    case ButtonClicked(_) => thisEditor.peer.stopCellEditing              // from checkbox
                }

                def setComponent(c: Component) {
                    if (lastComponent != null) {
                        deafTo(lastComponent)
//                        contents -= lastComponent
                        peer.remove(lastComponent.peer) // remove all child component
                    }
                    c match {
                        case maskField: MaskedTextField => listenTo(maskField)
                        case textField: TextField => listenTo(textField)
                        case comboBox: ComboBox[_] => listenTo(comboBox.selection)
                        case checkBox: CheckBox => listenTo(checkBox)
                        case _ => listenTo(c)
                    }
//                    contents += c
                    peer.add(c.peer)
                    lastComponent = c
                }
            }

            var currentNode: XsdNode = null
            override def componentFor(tree: Tree[_], a: XsdNode, info: companion.CellInfo): Component = {
                    currentNode = a
                    if (a.isEditable) {
                        cellEditor.setComponent(a.editor)
                        cellEditor.label.text = a.elmt + ":"
                        a.setEditValue(a.value)
                        cellEditor
                    }
                    else null
                }
            override def value = {
                val node = XsdNode(new XSElementDecl(None))
                node.value = currentNode.getEditValue
                node
            }
            override def cellEditable = true

//            override def fireCellEditingCancelled() { Console.out.println("editing cancelled") }
//            override def fireCellEditingStopped() { Console.out.println("editing stopped") }

        }
        showsRootHandles = true
    }

    viewportView = xsdTree
}
