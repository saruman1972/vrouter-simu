package com.allinfinance.vrouter.simu

import scala.swing._
import Swing._
import collection.JavaConversions._

import javax.swing.UIManager
import javax.swing.plaf.FontUIResource
import javax.swing.SwingUtilities

import com.thoughtworks.xstream.XStream
import com.thoughtworks.xstream.io.xml.DomDriver

import com.allinfinance.xsd._

import javax.swing.JTextPane
import javax.swing.text.StyledDocument
import javax.swing.text.AttributeSet
import javax.swing.text.SimpleAttributeSet
import javax.swing.text.StyleConstants
import java.awt.Color

import Util._

class TextPane extends Component {
    override lazy val peer: JTextPane = new JTextPane

    def doc: StyledDocument = peer.getStyledDocument

    def insert(offset: Int, s: String, attr: AttributeSet) = doc.insertString(offset, s, attr)

    def append(s: String, attr: AttributeSet=null) = insert(doc.getLength, s, attr)

    def text: String = doc.getText(0, doc.getLength)
    def text_=(s: String) = {
        doc.remove(0, doc.getLength)
        append(s, null)
    }
}

// object Main {
//     def main(args: Array[String]): Unit = {
//         import XsdNode.generateMAC

//         val msg = "0200||000000|            |||          ||||      |    |    |    |    |||    ||||   |||  |  ||         |||||||||            ||||        |               |                                        ||||||   |||        |                |||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||abcdefgh"
// //        val msg = "ABCDEF0123456789ABCD"
//         println(generateMAC("ABCDEF0123456789", msg))
//     }
// }

case class VRouterConfig(val ip: String, val port: Int, val pik: String, val mak: String, val zmk: String)
case class PathValuePair(val path: String, val value: String)
case class CaseValue(val desc: String, val xsd: String, val values: java.util.List[PathValuePair])

object VRouterSimu extends SimpleSwingApplication {
//UIManager.getDefaults.keys.foreach {key => 
//val value = UIManager.get(key)
//if (value.isInstanceOf[FontUIResource])
//Console.out.println("key=" + key + ", value=" + value)
//                                }
//UIManager.put("swing.boldMetal", false)
val font = UIManager.get("TextField.font")
UIManager.put("RadioButton.font", font)
UIManager.put("Menu.font", font)
UIManager.put("CheckBox.font", font)
UIManager.put("MenuItem.font", font)
UIManager.put("PopupMenu.font", font)
UIManager.put("ComboBox.font", font)
UIManager.put("Label.font", font)

    var config = {
        val s = scala.io.Source.fromFile("vrouter-config.xml").mkString
        val xstream = new XStream(new DomDriver)
        xstream.alias("vrouter-config", classOf[VRouterConfig])
        xstream.fromXML(s).asInstanceOf[VRouterConfig]
    }

//    val ipTextField = new MaskedTextField("###.###.###.###")
    val ipTextField = new TextField(config.ip)
    ipTextField.peer.setColumns(15)
//    val portTextField = new MaskedTextField("#####")
    val portTextField = new TextField(config.port.toString)
    portTextField.peer.setColumns(5)
//    val pikTextField = new MaskedTextField("HHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHH")
    val pikTextField = new TextField(config.pik)
    pikTextField.peer.setColumns(32)
    val makTextField = new TextField(config.mak)
    makTextField.peer.setColumns(32)
    val zmkTextField = new TextField(config.zmk)
    zmkTextField.peer.setColumns(32)

    def saveConfig(config: VRouterConfig): Unit = {
        import java.io.PrintWriter
        val out = new PrintWriter("vrouter-config.xml")
            
        val xstream = new XStream(new DomDriver)
        xstream.alias("vrouter-config", classOf[VRouterConfig])
        val content = xstream.toXML(config)
        try { out.print(content) } finally {out.close}
    }

    def updateMak(config: VRouterConfig, new_mak: String): Unit = {
        makTextField.text = new_mak
        val new_cfg = config.copy(mak = new_mak)
        saveConfig(new_cfg)
    }

    def updatePik(config: VRouterConfig, new_pik: String): Unit = {
        pikTextField.text = new_pik
        val new_cfg = config.copy(pik = new_pik)
        saveConfig(new_cfg)
    }

    val errorAttr = {
        val attr = new SimpleAttributeSet
        StyleConstants.setBold(attr, true)
        StyleConstants.setForeground(attr, new Color(255,0,0))
        attr
    }

    class ServiceInvoker(val textArea: TextPane, val sendBtn: Button, val respXsd: XsdNode, val config: VRouterConfig) {
        import java.net.{Socket, InetSocketAddress}
        import java.io._

        val sock = new Socket()
        val buf = new Array[Byte](10240)
        var receiveThread: Thread = null

        def printStackTrace(e: Throwable): Unit = {
            val sw = new StringWriter
            e.printStackTrace(new PrintWriter(sw))
            textArea.text += sw
        }

        def send(msg: (Vector[Byte], String)): Boolean = {
            try {
                val (message, log) = msg
                val dispMsg = new String(message.toArray, "GBK")
                sendBtn.enabled = false
                textArea.text += "send message: =====>\n[" + dispMsg + "]\n" + log + "\n"
println("send message: ======>\n[" + dispMsg + "]\n" + log + "\n")

                sock.connect(new InetSocketAddress(config.ip, config.port))
                val out = sock.getOutputStream
                val bytes = message.toArray
                out.write("%06d".format(bytes.size).getBytes)
                out.write(bytes)

                receiveThread = new Thread {
                    override def run: Unit = {
                        try {
                            val in = sock.getInputStream
                            in.read(buf, 0, 6)
                            val len = (new String(buf, 0, 6)).toInt
println("read length=" + (new String(buf, 0, 6)) + ", len=" + len)
                            var offset = 0
                            var n = 0
                            do {
                                n = in.read(buf, offset, len-offset)
                                if (n > 0) offset += n
                            } while((offset < len) && (n >= 0))
                            val ret = Vector[Byte]() ++ java.util.Arrays.copyOfRange(buf, 0, len)
println("receive message: =====>\n[" + (new String(ret.toArray, "GBK")) + "]\n")
                            val (dtl, new_key, _) = XsdNode.unpackRoot(respXsd, ret, config.mak, config.zmk)
                            
                            SwingUtilities.invokeLater(new Runnable {
                                override def run: Unit = {
                                    textArea.append("receive message: =====>\n[" + (new String(ret.toArray, "GBK")) + "]\n")
                                    dtl.foreach {line =>
                                        line match {
                                            case Right(msg) => textArea.append(msg)
                                            case Left(msg)  => textArea.append(msg, errorAttr)
                                        }
                                             }
                                    new_key match {
                                        case Some(("MAK", new_mak)) =>
                                            updateMak(config, new_mak)
                                            textArea.append("MAK 更新成功\n\n")
                                        case Some(("PIK", new_pik)) =>
                                            updatePik(config, new_pik)
                                            textArea.append("PIK 更新成功\n\n")
                                        case _ => 
                                    }
                                    sendBtn.enabled = true
                                }
                            })
                        }
                        catch {
                            case e =>
                                Dialog.showMessage(null, e.formatException)
                                sendBtn.enabled = true
                        }
                        finally {sock.close}
                    }
                }
                receiveThread.start
                true
            }
            catch {
                case e =>
                    sendBtn.enabled = true
                    sock.close
                    printStackTrace(e)
                    false
            }
        }

        def cancel: Unit = {
            if (receiveThread != null) {
                receiveThread.stop
                receiveThread = null
            }
            sendBtn.enabled = true
        }
    }

    def top = new MainFrame {
        title = "vrouter simulator"
        var currentXsd: String = null
        val caseLabel = new Label("NonFinance Transaction:")
        val tree = new XsdTree
        var serviceInvoker: ServiceInvoker = null
        val bottom = new BoxPanel(Orientation.Horizontal) {
            contents += new Button(Action("open") {
                val chooser = new FileChooser {
                    peer.setCurrentDirectory(new java.io.File(System.getProperty("user.dir")+"/xsd"))
                }
                chooser.fileSelectionMode = FileChooser.SelectionMode.FilesOnly
                if (chooser.showOpenDialog(this) == FileChooser.Result.Approve) {
                    caseLabel.text = "NonFinance Transaction:" + chooser.selectedFile.getPath
                    if (tree.setFile(chooser.selectedFile.getPath)) {
                        currentXsd = chooser.selectedFile.getPath
                        sendBtn.enabled = true
                    }
                    else {
                        sendBtn.enabled = false
                    }
                }
            })
            val sendBtn: Button = new Button(Action("send") {
scala.Console.out.println("ip=" + ipTextField.text)
scala.Console.out.println("port=" + portTextField.text)
scala.Console.out.println("pik=" + pikTextField.text)
scala.Console.out.println("mak=" + makTextField.text)
scala.Console.out.println("zmk=" + zmkTextField.text)
                config = VRouterConfig(ipTextField.text, portTextField.text.toInt, pikTextField.text, makTextField.text, zmkTextField.text)

                val serviceNode = tree.xsdTree.model.roots(0)
                XsdNode.clearAutoGeneratedField(serviceNode)
                XsdNode.fillAutoGenerateField(serviceNode, config.pik)
                val msg = XsdNode.packRoot(serviceNode, makTextField.text)

                serviceInvoker = new ServiceInvoker(right, sendBtn, tree.respXsd, config)
                serviceInvoker.send(msg)
            })
            sendBtn.enabled = false
            contents += sendBtn
            val cancelBtn = new Button(Action("cancel") {
                if (serviceInvoker != null) {
                    serviceInvoker.cancel
                    serviceInvoker = null
                }
            })
            contents += cancelBtn
            contents += new Button(Action("clear") {
                right.text = ""
            })
            contents += new Label("    ")
            // contents += new Button(Action("save as") {
            //     val chooser = new FileChooser {
            //         peer.setCurrentDirectory(new java.io.File(System.getProperty("user.dir")+"/cases"))
            //     }
            //     chooser.fileSelectionMode = FileChooser.SelectionMode.FilesOnly
            //     if (chooser.showSaveDialog(this) == FileChooser.Result.Approve) {
            //         val xstream = new XStream(new DomDriver)
            //         xstream.alias("vrouter-case", classOf[CaseValue])
            //         xstream.alias("entry", classOf[PathValuePair])
            //         val serviceNode = tree.xsdTree.model.roots(0)
            //         val pvs = new java.util.ArrayList[PathValuePair]()
            //         XsdNode.serializeInputValue(serviceNode).foreach {x => pvs.add(PathValuePair(x._1, x._2))}
            //         val cv = CaseValue(chooser.selectedFile.getPath, currentXsd, pvs)
            //         val s = xstream.toXML(cv)
            //         Some(new java.io.PrintWriter(chooser.selectedFile.getPath)).foreach{p =>
            //             p.write(s)
            //             p.close
            //         }
            //     }
            // })
//             contents += new Button(Action("load") {
//                 val chooser = new FileChooser {
//                     peer.setCurrentDirectory(new java.io.File(System.getProperty("user.dir")+"/cases"))
//                 }
//                 chooser.fileSelectionMode = FileChooser.SelectionMode.FilesOnly
//                 if (chooser.showOpenDialog(this) == FileChooser.Result.Approve) {
//                     val s = scala.io.Source.fromFile(chooser.selectedFile.getPath).mkString

//                     val xstream = new XStream(new DomDriver)
//                     xstream.alias("vrouter-case", classOf[CaseValue])
//                     xstream.alias("entry", classOf[PathValuePair])
//                     val cv = xstream.fromXML(s).asInstanceOf[CaseValue]
//                     tree.setFile(cv.xsd)
//                     val serviceNode = tree.xsdTree.model.roots(0)
// //                    XsdNode.fillLoadedValues(serviceNode, cv.values.map {x => (x.path, x.value)} .toList)
//                     cv.values.foreach {x =>
//                         val node = XsdNode(new XSElementDecl(None))
//                         node.value = x.value
//                         XsdNode.stringPathToTreePath(serviceNode, x.path).map {path => tree.xsdTree.model.update(path, node)}
//                     }
//                 }
//             })
//            contents += new Label("    ")
            contents += new BoxPanel(Orientation.Vertical) {
                contents += new FlowPanel {
                    contents += new Label("ip:")
                    contents += ipTextField
                    contents += new Label("port:")
                    contents += portTextField
                }
                contents += new FlowPanel {
                    contents += new Label("pik:")
                    contents += pikTextField
                }
                contents += new FlowPanel {
                    contents += new Label("mak:")
                    contents += makTextField
                }
                contents += new FlowPanel {
                    contents += new Label("zmk:")
                    contents += zmkTextField
                }
            }
        }
//        val right = new TextArea()
        val right = new TextPane()

        tree.preferredSize = new java.awt.Dimension(600, 400)
//        right.minimumSize = new java.awt.Dimension(400, 600)
        contents = new BoxPanel(Orientation.Vertical) {
            contents += caseLabel
            contents += new BoxPanel(Orientation.Horizontal) {
                val scrollTextArea = new ScrollPane(right)
//                scrollTextArea.horizontalScrollBarPolicy = ScrollPane.BarPolicy.AsNeeded
//                scrollTextArea.verticalScrollBarPolicy = ScrollPane.BarPolicy.AsNeeded
                contents += new SplitPane(Orientation.Vertical, tree, scrollTextArea) {
                    continuousLayout = true
                }
            }
            contents += bottom
        }
        minimumSize = new java.awt.Dimension(1100, 600)
    }
}
