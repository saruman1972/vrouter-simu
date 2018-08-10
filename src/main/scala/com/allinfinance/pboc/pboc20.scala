package com.allinfinance.pboc20

import org.apache.commons.codec.binary.Hex

object Pboc20 {
    def packTag(tag: String, value: Array[Byte]) = {
        val buf = 
        if (value.length > 127)
            Vector[Byte](0x81.toByte, value.length.toByte)
        else
            Vector[Byte](value.length.toByte)
        buf ++ value
    }

    def unpackTag(buf: String) = {
    }
}


