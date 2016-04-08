package com.iofficecorp.jasperserver

import com.iofficecorp.jasperserver.pumped.`package`._

import scala.xml.NamespaceBinding


/**
 * The valid namespaces for marshalling and unmarshalling Jasper's xml resources.
 */
sealed trait Namespace
object Namespace {
  def DOMAIN: NamespaceBinding = scalaxb.toScope(Some("jsdomain") -> defaultScope.getURI("jsdomain"))
  def REPORT: NamespaceBinding = scalaxb.toScope(Some("jr") -> defaultScope.getURI("jr"))
}
