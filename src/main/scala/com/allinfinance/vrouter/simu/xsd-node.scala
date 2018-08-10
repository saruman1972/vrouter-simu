package com.allinfinance.vrouter.simu

import scala.swing._
import scala.swing.event._
import scala.util.matching.Regex

import javax.swing.JFormattedTextField
import javax.swing.text.MaskFormatter
import java.awt.event.FocusAdapter

import javax.crypto.Cipher
import javax.crypto.SecretKey
import javax.crypto.SecretKeyFactory
import javax.crypto.spec.DESedeKeySpec
import javax.crypto.spec.IvParameterSpec

import org.apache.commons.codec.binary.Hex
import java.util.Arrays

import com.allinfinance.xsd._

case class MaskEditDone(override val source: MaskedTextField) extends ValueChanged(source)
class MaskedTextField(mask: String) extends FormattedTextField(null) {
    override lazy val peer: JFormattedTextField = new JFormattedTextField(new MaskFormatter(mask)) with SuperMixin
import java.awt.Font
val ft = new Font( "Monospaced", Font.PLAIN, 12 )
peer.setFont(ft)
// peer.setColumns(19)

    private lazy val actionListener = Swing.ActionListener { e =>
        publish(MaskEditDone(MaskedTextField.this))
                                                        }

    protected override def onFirstSubscribe() {
        super.onFirstSubscribe
        peer.addActionListener(actionListener)
        peer.addFocusListener(new FocusAdapter {
            override def focusLost(e: java.awt.event.FocusEvent) { publish(MaskEditDone(MaskedTextField.this)) }
        })
    }

    protected override def onLastUnsubscribe() {
        super.onLastUnsubscribe
        peer.removeActionListener(actionListener)
    }
}

class PatternedTextField(val pattern: Regex, maxLength: Int=10) extends TextField {
    private var effPattern = pattern
    val doc = new javax.swing.text.PlainDocument {
        override def insertString(index: Int, s: String, a: javax.swing.text.AttributeSet) {
            val newString = getText(0, index) + s + getText(index, getLength-index)
            if (effPattern != null) {
                for (_ <- effPattern.findFirstIn(newString)) {
                    if (newString.length <= maxLength)
                        super.insertString(index, s, a)
                }
            }
            else if (newString.length <= maxLength)
                super.insertString(index, s, a)
        }
        override def remove(index: Int, len: Int) {
            val newString = getText(0, index) + getText(index+len, getLength-index-len)
            if (effPattern != null)
                for (_ <- effPattern.findFirstIn(newString)) {
                    super.remove(index, len)
                }
            else
                super.remove(index, len)
        }
    }

import java.awt.Font
val ft = new Font( "Monospaced", Font.PLAIN, 12 )
peer.setFont(ft)
    peer.setDocument(doc)
    peer.setColumns(maxLength)

    override def text_=(s: String): Unit = {
        effPattern = null
        doc.remove(0, doc.getLength)
        super.text = s
        effPattern = pattern
    }
}

class XsdNode (val elmt: XSElementDecl) {
    var value: String = ""
    var transformedValue: String = ""
    override def toString = elmt.toString + ":" + (if (hasInputValue) actualValue else "")
    val children = new collection.mutable.ArrayBuffer[XsdNode]()
    var parent: Option[XsdNode] = None
    def add(node: XsdNode): Unit = {
        children += node
        node.parent = Some(this)
    }

    def setEditValue(value: String): Unit = {
        editor match {
            case comboBox: ComboBox[String] => comboBox.selection.item = value
            case t: PatternedTextField => elmt.elmtType match {
                case Some(tp) =>
                    if (tp.derivedFrom(XSBuiltinDate)) {
                        if (value == "") t.text = "--" else t.text = value
                    }
                    else t.text = value
                case _ => t.text = value
            }
            case m: MaskedTextField => m.text = value
        }
    }
    def getEditValue: String = {
        editor match {
            case comboBox: ComboBox[String] => 
                try {
                    comboBox.selection.item.asInstanceOf[(String,String)]._1
                } catch { case _ => "" }
            case t: PatternedTextField => elmt.elmtType match {
                case Some(tp) =>
                    if (tp.derivedFrom(XSBuiltinDate)) {
                        if (value == "--") "" else t.text
                    }
                    else t.text
                case _ => t.text
            }
            case m: MaskedTextField => m.text
        }
    }
    lazy val editor: Component = {
        elmt.elmtType match {
            case Some(tp) => tp match {
                case XSBuiltinDate => new MaskedTextField("####-##-##")
                case XSBuiltinTime => new MaskedTextField("##:##:##")
                case XSBuiltinDatetime => new MaskedTextField("####-##-##T##:##:##")
                case bt: XSBuiltinType => new PatternedTextField(bt.pattern)
                case st: XSSimpleType =>
                    st.facetsMap.get("enumeration") match {
                        case Some(enums) => new ComboBox(enums.map{
                            f =>
                                val e = f.asInstanceOf[XSFacetEnumeration]
                                val desc = (for (ann <- e.annotation;
                                                doc <- ann.documentation)
                                                yield doc) getOrElse ("")
                                (e.value, desc)
                        })
                        case None => st.baseType match {
                            case XSBuiltinDate => new MaskedTextField("####-##-##")
                            case XSBuiltinTime => new MaskedTextField("##:##:##")
                            case XSBuiltinDatetime => new MaskedTextField("####-##-##T##:##:##")
                            case _ => elmt.actualFixedValue match {
                                case None => new PatternedTextField(st.pattern, if (st.maxLength < 0) 999 else st.maxLength)
                                case Some(_) => null
                            }
                        }
                    }
                case _ => null
            }
            case _ => null
        }
    }
    def isEditable: Boolean = editor != null
    def hasInputValue: Boolean = ((value != "") || (transformedValue != "") || (elmt.actualFixedValue != None) || (children.filter(x => x.hasInputValue).size > 0))
    def actualValue: String = elmt.actualFixedValue match {
        case Some(v) => v
        case None =>
            if (transformedValue != "")
                transformedValue
            else
                value
    }
    def pad(s: Vector[Byte], len: Int, ch: Byte): Vector[Byte] = Vector.fill(math.max(if (len > 0) len else s.length, s.length) - s.length)(ch)
    def padLeft(s: Vector[Byte], len: Int, ch: Byte): Vector[Byte] = pad(s, len, ch) ++ s
    def padRight(s: Vector[Byte], len: Int, ch: Byte): Vector[Byte] = s ++ pad(s, len, ch)
    def packValue: Vector[Byte] = {
        val v = Vector.empty[Byte] ++ (if (hasInputValue) actualValue else "").getBytes("GBK")
        elmt.elmtType match {
            case Some(tp) => tp match {
                case st: XSSimpleType => 
                    st.facetsMap.get("enumeration") match {
                        case Some(enums) =>
                            val vs = enums.map{f => f.asInstanceOf[XSFacetEnumeration].value}
                            padLeft(v, vs(0).length, ' '.toByte)
                        case None => st.ancestor match {
                            case _: XSBuiltinDecimalTrait => padLeft(v, st.length, '0'.toByte)
                            case _ => padRight(v, st.length, ' '.toByte)
                        }
                    }
                case _ => v
            }
            case _ => v
        }
    }

    private def unpackBuf(msg: Vector[Byte], len: Int): (String, Vector[Byte]) = {
        val (t,m) = msg.splitAt(len)
        (new String(t.toArray, "GBK"), m)
    }
    def unpackValue(msg: Vector[Byte]): (Either[String,String], Vector[Byte]) = {
        elmt.actualFixedValue match {
            case Some(v) =>
                val (t,m) = unpackBuf(msg, v.length)
                (if (v == t) Right(t) else Left(t), m)
            case None =>
                elmt.elmtType match {
                    case Some(tp) => tp match {
                        case st: XSSimpleType => 
                            st.facetsMap.get("enumeration") match {
                                case Some(enums) =>
                                    val vs = enums.map{f => f.asInstanceOf[XSFacetEnumeration].value}
                                    val (v,m) = unpackBuf(msg, vs(0).length)
                                    vs.find{_ == v} match {
                                        case Some(e) => (Right(v), m)
                                            case None => (Left(v), m)
                                    }
                                case None => 
                                    val (v,m) = unpackBuf(msg, st.length)
                                    (if (v.getBytes("GBK").length == st.length) Right(v) else Left(v), m)
                            }
                        case _ => (Right(""), msg)
                    }
                    case _ => (Right(""), msg)
                }
        }
    }
}

object XsdNode {
    def apply(value: XSElementDecl) = new XsdNode(value)
    def buildSchemaTree(schema: XSSchema): XsdNode = {
        val root = XsdNode(new XSElementDecl(None))
        schema.rootElement map {elmt => buildElement(elmt, root)}
        root.children(0)
    }
    def buildElement(elmt: XSElementDecl, parent: XsdNode): XsdNode = {
        def buildElementHelper(elmt: XSElementDecl, node: XsdNode): XsdNode = {
            elmt.nameOrRef match {
                case Some(r @ XSReference(_)) =>
                    buildElementHelper(r.ref.get, node)
                case _ =>
                    elmt.elmtType match {
                        case Some(t) => t match {
                            case ct : XSComplexType => ct.particle match {
                                case Some(g : XSGroupDecl) => buildGroupDecl(g, node)
                                case Some(a : XSGroup) => buildGroup(a, node)
                                case _ =>
                            }
                            case _ => // simple type, no need to create node
                        }
                        case None => throw new IllegalArgumentException("element[" + elmt + "] " + "node[" + node + "] has no type defined")
                    }
            }
            node
        }
        val node = XsdNode(elmt)
        parent.add(node)
        buildElementHelper(elmt, node)
    }
    def buildGroupDecl(group: XSGroupDecl, parent: XsdNode): Unit = {
        group.nameOrRef match {
            case Some(r @ XSReference(_)) => buildGroupDecl(r.ref.get, parent)
            case _ => buildGroup(group.group.get, parent)
        }
    }
    def buildGroup(group: XSGroup, parent: XsdNode): Unit = {
        group.children.foreach {particle =>
            particle match {
                case elmt: XSElementDecl => buildElement(elmt, parent)
                case g: XSGroupDecl => buildGroupDecl(g, parent)
                case g: XSGroup => buildGroup(g, parent)
                case any: XSAny => 
            }
        }
    }

    var delimeter: String = ""
    var xsdRoot: XsdNode = null
    def packRoot(root: XsdNode, mak: String): (Vector[Byte], String) = {
        delimeter = ""
        xsdRoot = root
        val (msg,log) = pack(root, 0)
        val head_without_mac = msg.take(149)
        val body = msg.slice(149+32, msg.length)
        val mac =
            if (new String(head_without_mac.take(4).toArray) == "SS01") {
                if (new String(body.slice(16,19).toArray) == "001") // MAK
                    Vector.fill(4)(0xEE.toByte)
                else
                    generateKeyMAC(mak, body).take(4)
            }
            else
                generateTransMAC(mak, body).take(4)
        val packMac = Hex.encodeHex(mac.toArray).map {_.toUpper.toByte} ++ Vector.fill(24)(' '.toByte)
val v=        (head_without_mac ++ packMac ++ body, "MAC: \\[ {32}\\]".r.replaceAllIn(log, "MAC: [" + (new String(packMac)) + "]"))
        v
    }
    def pack(node: XsdNode, tab: Int=4): (Vector[Byte], String) = {
        def packChildren(tab: Int): (Vector[Byte], String) = {
            val (v, log) = node.children.foldLeft((Vector.empty[Byte],"")){
                (accum,x) =>
                    val (a,b) = pack(x, tab+4)
                    (accum._1 ++ a, accum._2 + b)
            }
            (v, " " * tab + node.elmt.actualName + ":\n" + log)
        }
        def pack8583(tab: Int): (Vector[Byte], String) = {
            val (v, log) = node.children.foldLeft(((Vector.empty[Byte], 1),"")) {
                (accum,x) =>
                    val ((acc,idx),log) = accum
                    val Pattern = "B([0-9]{3})".r
                    val (pad,curr_idx) = x.elmt.elmtType match {
                        case Some(XSSimpleType(Some(tp))) => tp match {
                            case Pattern(fld) => (delimeter * (fld.toInt - idx - 1), fld.toInt)
                            case _ => ("", idx)
                        }
                        case _ => ("", idx)
                    }
                    val (a,b) = pack(x, tab+4)
                    ((acc ++ pad.getBytes ++ a, curr_idx), log + b)
            }
            // extra 1 for trailing "|"
            (v._1 ++ (delimeter * (128 - v._2 + 1)).getBytes, (" " * tab) + node.elmt.actualName + ":\n" + log)
        }

        if (node.children.size > 0) { // complexType
            if (node.elmt.actualName == "REQUEST") {
                val header = findTag(xsdRoot, "SERVICE_HEADER").get
                findTag(header, "SERVICE_ID") match {
                    case Some(x) =>
                        new String(x.packValue.toArray) match {
                            case "0001" =>
                                delimeter = "|"
                                pack8583(tab)
                            case _ => packChildren(tab)
                        }
                    case None => packChildren(tab)
                }
            }
            else
                packChildren(tab)
        }
        else {
            val v = node.packValue
            (Vector.empty[Byte] ++ delimeter.getBytes ++ v, (" " * tab) + node.elmt.actualName + ": [" + (new String(v.toArray, "GBK")) + "]\n")
        }
    }

    private def findTag(node: XsdNode, tag: String): Option[XsdNode] = {
        node.children.find {x => x.elmt.actualName == tag}
    }
    var dateFormater = new java.text.SimpleDateFormat("yyyyMMdd")
    def effKey(key: String): String = key.size match {
        case 16 => key + key + key
        case 32 => key + key.substring(0, 16)
        case 48 => key
        case _ => throw new IllegalArgumentException("invalid key length")
    }
    def encryptPIN(key: String, pin: String, cardno: Option[String]): String = {
        val keySpec = new DESedeKeySpec(Hex.decodeHex(effKey(key).toArray))
        val secretKey = SecretKeyFactory.getInstance("DESede").generateSecret(keySpec)
        val pinpadded = Hex.decodeHex(("%02d%s".format(pin.size, pin) + "F"*(16-2-pin.size)).toArray)
Console.out.println("pinpadded=" + Hex.encodeHex(pinpadded).mkString)
        val acctno = Hex.decodeHex((cardno match {
            case Some(cn) if (cn.size > 13) => "0000" + cn.substring(cn.size-1-12, cn.size-1)
            case _ => "0"*16
        }).toArray)
Console.out.println("acctno=" + Hex.encodeHex(acctno).mkString)
        val pinblock = pinpadded.zip(acctno).map {case (a,b) => (a ^ b).toByte}.toArray
Console.out.println("pinblock=" + Hex.encodeHex(pinblock).mkString)
        val cipher = Cipher.getInstance("DESede")
        cipher.init(Cipher.ENCRYPT_MODE, secretKey)
        Hex.encodeHex(cipher.doFinal(pinblock)).mkString.substring(0, 16).toUpperCase
    }
    def generateKeyMAC(key: String, msg: Vector[Byte]): Vector[Byte] = {
        generateMAC(key, msg)
    }
    def generateTransMAC(key: String, msg: Vector[Byte]): Vector[Byte] = {
        generateMAC(key, msg)
    }
    def generateMAC(key: String, msg: Vector[Byte]): Vector[Byte] = {
        val keySpec = new DESedeKeySpec(Hex.decodeHex(effKey(key).toArray))
        val secretKey = SecretKeyFactory.getInstance("DESede").generateSecret(keySpec)
        val cipher = Cipher.getInstance("DESede/CBC/PKCS5Padding")
        val iv = new IvParameterSpec(("\0" * 8).getBytes)
        cipher.init(Cipher.ENCRYPT_MODE, secretKey, iv)
        val blocks = (msg ++ Vector.fill(8 - (msg.length % 8 match {case 0 => 8 case x => x}))(0x00.toByte)).grouped(8).toList
        blocks.init.foreach {
            blk =>
                cipher.update(blk.toArray)
        }
        (Vector.empty[Byte] ++ cipher.doFinal(blocks.last.toArray)).take(8)
    }
    def clearAutoGeneratedField(node: XsdNode): Unit = {
        node.transformedValue = ""
        for (n <- node.children)
            clearAutoGeneratedField(n)
    }
    def fillAutoGenerateField(root: XsdNode, pik: String): Unit = {
        val now = java.util.Calendar.getInstance().getTime()
        val header = findTag(root, "SERVICE_HEADER").get
        findTag(header, "SERVICE_SN").map {x => x.transformedValue = "%015d".format(now.getTime).substring(3)}
        findTag(header, "BANK_DATE").map {x => x.transformedValue = dateFormater.format(now)}
        val body = findTag(root, "SERVICE_BODY").get
        val request = findTag(body, "REQUEST").get
        val cardno = findTag(request, "CARD_NO").map(x => x.value)
        for (pin <- findTag(request, "PIN");
             if (pin.value != "")) {
                 pin.transformedValue = encryptPIN(pik, pin.value, cardno)
             }
        for (pin <- findTag(request, "NEW_PIN");
             if (pin.value != "")) {
                 pin.transformedValue = encryptPIN(pik, pin.value, cardno)
             }
        for (qpin <- findTag(request, "OLD_PIN");
             if (qpin.value != "")) {
                 qpin.transformedValue = encryptPIN(pik, qpin.value, cardno)
             }
        for (qpin <- findTag(request, "QRY_PIN");
             if (qpin.value != "")) {
                 qpin.transformedValue = encryptPIN(pik, qpin.value, cardno)
             }
        for (qpin <- findTag(request, "TRANS_PIN");
             if (qpin.value != "")) {
                 qpin.transformedValue = encryptPIN(pik, qpin.value, cardno)
             }
        for (qpin <- findTag(request, "PIN_BLOCK");
             if (qpin.value != "")) {
                 qpin.transformedValue = encryptPIN(pik, qpin.value, cardno)
             }
    }

    def serializeInputValue(root: XsdNode): List[(String, String)] = {
        def serializeHelper(path: String, node: XsdNode): List[Option[(String, String)]] = {
            val currPath =
                if (path == "")
                    node.elmt.toString
                else
                    path + "." + node.elmt.toString
            if (node.children.size > 0)
                node.children.toList.flatMap {n => serializeHelper(currPath, n)}
            else if (node.value != "")
                List(Some((currPath, node.value)))
            else
                List(None)
        }

        for (option <- serializeHelper("", root); x <- option) yield x
    }

    def stringPathToTreePath(root: XsdNode, path: String): Option[IndexedSeq[XsdNode]] = {
            path.split("""\.""").foldLeft(Some((List[XsdNode](), List(root))): Option[(List[XsdNode], List[XsdNode])]) {(ps, p) =>
                ps match {
                    case Some((pt, nodes)) => nodes.find {x => x.elmt.toString == p} map {x => (pt :+ x, x.children.toList)}
                    case None => None
                }
            } map {p => p._1.toIndexedSeq}
    }
    def fillLoadedValues(root: XsdNode, values: List[(String, String)]): Unit = {
        values.foreach {case (path, value) =>
            path.split("""\.""").foldLeft(Some(null, List(root)): Option[(XsdNode, List[XsdNode])]) {(ln, p) =>
                ln match {
                    case Some((_, nodes)) => nodes.find {x => x.elmt.toString == p} map {x => (x, x.children.toList)}
                    case None => None
                }
            } map {case (n, _) => Console.out.println(n + "=" + value); n.value = value}
        }
    }

    def unpackRoot(node: XsdNode, msg: Vector[Byte], mak: String, zmk: String): (Vector[Either[String,String]], Option[(String, String)], Vector[Byte]) = {
        val (log, remain) = unpack(node, msg, 0)
        val head_without_mac = msg.take(149)
        val mac_in_msg = new String(msg.slice(149, 149+8).toArray)
        val body = msg.slice(149+32, msg.length)
println(new String(body.toArray, "GBK"))
        val (mac,new_key) =
            if (new String(head_without_mac.take(4).toArray) == "SS01") {
                val nkey_under_zmk = new String(body.slice(8+8+3, 8+8+3+48).toArray).trim

println("zmk=" + zmk)
                val keySpec = new DESedeKeySpec(Hex.decodeHex(effKey(zmk).toArray))
                val secretKey = SecretKeyFactory.getInstance("DESede").generateSecret(keySpec)
                val cipher = Cipher.getInstance("DESede/ECB/NoPadding")
                cipher.init(Cipher.DECRYPT_MODE, secretKey)
println("nkey_under_zmk=" + nkey_under_zmk)
                val nkey = Hex.encodeHex(cipher.doFinal(Hex.decodeHex(nkey_under_zmk.toArray))).mkString.toUpperCase
println("nkey=" + nkey)

                if (new String(body.slice(16,19).toArray) == "001") { // MAK
                    (generateTransMAC(nkey, body).take(4), Some(("MAK", nkey)))
                }
                else
                    (generateTransMAC(mak, body).take(4), Some(("PIK", nkey)))
            }
            else
                (generateTransMAC(mak, body).take(4), None)
        val mac_calculated = new String(Hex.encodeHex(mac.toArray).map {_.toUpper.toByte})
println("mac_in_msg=[" + mac_in_msg + "], mac_calculated=[" + mac_calculated + "]")
        if (mac_in_msg == mac_calculated)
            (log, new_key, remain)
        else
            (log :+ Left("mac check failed! calculated value is [" + mac_calculated + "]\n"), None, remain)

    }
    def unpack(node: XsdNode, msg: Vector[Byte], tab: Int=4): (Vector[Either[String,String]], Vector[Byte]) = {
        def unpackChildren(tab: Int): (Vector[Either[String,String]], Vector[Byte]) = {
            val (v, new_msg) = node.children.foldLeft((Vector.empty[Either[String,String]], msg)){(accum,x) =>
                val (a, b) = unpack(x, accum._2, tab+4)
                (accum._1 ++ a, b)}
            (Right(" " * tab + node.elmt.actualName + ":\n") +: v, new_msg)
        }
        def unpack8583(tab: Int): (Vector[Either[String,String]], Vector[Byte]) = {
            (Right(" " * tab + node.elmt.actualName + ":\n") +: 
                (new String(msg.toArray, "GBK")).split("\\|").zipWithIndex.foldLeft(Vector.empty[Either[String,String]]) {
                    (accum,pair) =>
                        val (x, idx) = pair
                        x.length match {
                            case 0 => accum
                            case _ => accum :+ Right(" " * (tab+4) + "B%03d".format(idx) + ": [" + x + "]\n")
                        }
                }, Vector.empty[Byte])
        }
        
        if (node.children.size > 0) { // complexType
            if (node.elmt.actualName == "RESPONSE") {
                val header = findTag(xsdRoot, "SERVICE_HEADER").get
                findTag(header, "SERVICE_ID") match {
                    case Some(x) =>
                        new String(x.packValue.toArray) match {
                            case "0001" =>
                                unpack8583(tab)
                            case _ => unpackChildren(tab)
                        }
                    case None => unpackChildren(tab)
                }
            }
            else
                unpackChildren(tab)
        }
        else {
            val (v, new_msg) = node.unpackValue(msg)
            v match {
                case Right(a) => (Vector[Either[String,String]](Right((" " * tab) + node.elmt.actualName + ": [" + a + "]\n")), new_msg)
                case Left(b) =>  (Vector[Either[String,String]](Left((" " * tab) + node.elmt.actualName + ": [" + b + "] <=== error\n")), new_msg)
            }
        }
    }
}

