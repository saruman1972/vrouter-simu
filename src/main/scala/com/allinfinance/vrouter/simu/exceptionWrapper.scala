package com.allinfinance.vrouter.simu

object Util {
    class ExceptionWrapper(e: Throwable) {
        import java.io.{StringWriter, PrintWriter}
        def formatException: String = {
            val sw = new StringWriter
            e.printStackTrace(new PrintWriter(sw))
            sw.toString
        }
    }

    implicit def exception2Wrapper(e: Throwable) = new ExceptionWrapper(e)
}

