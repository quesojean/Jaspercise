package com.iofficecorp.jasperserver

import java.io.InputStream

import com.iofficecorp.jasperserver.pumped.domain.SchemaType
import com.iofficecorp.jasperserver.pumped.report.JasperReport

import scala.xml.{Elem, XML}

/**
 * Singleton that if needed, will return updated xml.
 * The factory will instantiate a particular Danceable case class dependant on the XML resourceStream's
 * namespace uri.
 */
object Jaspercise {

  //NOTE if we return the case class, its Product/Serializable type.
  def apply(resourceStream: InputStream, tableIds: Seq[String], fieldProps: Map[FieldProps, String]): Option[String] = {
    val resourceXML: Elem = XML.load(resourceStream)
    val domainNamespaceURI = Namespace.DOMAIN.uri
    val reportNamespaceURI = Namespace.REPORT.uri
    val builder: Option[Danceable[_ >: SchemaType with JasperReport <: Product with Serializable]] =
      resourceXML.namespace match {
      //stable identifier pattern?
      case `domainNamespaceURI` =>
        //domain builder- SchemaType
        //we could do Any or something to make the fromXML call?
        Some(DomainBuilder(resourceXML, tableIds, fieldProps))
      case `reportNamespaceURI` =>
        //adhoc or report builder - jasperReport (studio, custom reports?)
        Some(ReportBuilder(resourceXML, tableIds, fieldProps))
      case _ => None
    }
    if (builder.isDefined) {
      builder.get.workIt
    } else {
      None
    }
  }
}