package de.deloitte.logging

import java.io.{StringWriter, Writer}
import java.net.InetAddress
import java.text.DateFormat
import java.util.Date

import org.apache.log4j.Layout
import org.apache.log4j.helpers.ISO8601DateFormat
import org.codehaus.jackson.JsonFactory
import org.codehaus.jackson.map.MappingJsonFactory
import org.apache.log4j.spi.LoggingEvent

import scala.util.{Failure, Success, Try}

class ExtendedHadoopJsonLayout extends Layout {

  private[this] lazy val factory: JsonFactory = new MappingJsonFactory()

  private[this] lazy val sourceHost = Try(InetAddress.getLocalHost.getHostName) match {
    case Failure(_) => "localhost"
    case Success(value) => value
  }

  private[this] lazy val dateFormat: DateFormat = new ISO8601DateFormat()

  private[this] val VERSION_VALUE = 1

  private[this] val DATE = "date"
  private[this] val EXCEPTION_CLASS = "exception_class"
  private[this] val CLASS = "class"
  private[this] val LINE_NUMBER = "line_number"
  private[this] val LEVEL = "level"
  private[this] val MESSAGE = "message"
  private[this] val NAME = "name"
  private[this] val STACK = "stack"
  private[this] val THREAD_NAME = "thread_name"
  private[this] val SOURCE_HOST = "source_host"
  private[this] val TIMESTAMP = "@timestamp"
  private[this] val JSON_TYPE = "application/json"
  private[this] val VERSION = "@version"


  def toJson(writer: Writer, event: LoggingEvent): Writer = {
    val json = factory.createJsonGenerator(writer)
    val ts = event.getTimeStamp
    val ti = event.getThrowableInformation
    val li = event.getLocationInformation

    val date = new Date(ts)

    json.writeStartObject()
    json.writeStringField(NAME, event.getLoggerName)
    json.writeNumberField(TIMESTAMP, ts)
    json.writeStringField(DATE, dateFormat.format(date))
    json.writeStringField(LEVEL, event.getLevel.toString)
    json.writeStringField(THREAD_NAME, event.getThreadName)
    json.writeStringField(MESSAGE, event.getRenderedMessage)
    json.writeStringField(CLASS, li.getClassName)
    json.writeStringField(LINE_NUMBER, li.getLineNumber)
    json.writeNumberField(VERSION, VERSION_VALUE)
    json.writeStringField(SOURCE_HOST, sourceHost)

    if (ti != null) { //there is some throwable info, but if the log event has been sent over the wire,
      //there may not be a throwable inside it, just a summary.
      val thrown = ti.getThrowable
      val eclass = if (thrown != null) thrown.getClass.getName
      else ""
      json.writeStringField(EXCEPTION_CLASS, eclass)
      val stackTrace = ti.getThrowableStrRep
      json.writeArrayFieldStart(STACK)
      for (row <- stackTrace) {
        json.writeString(row)
      }
      json.writeEndArray()
    }
    json.writeEndObject()
    json.flush()
    json.close()
    return writer
  }

  override def getContentType: String = JSON_TYPE

  override def format(event: LoggingEvent): String = toJson(new StringWriter(), event).toString

  override def ignoresThrowable(): Boolean = false

  override def activateOptions(): Unit = {}
}
