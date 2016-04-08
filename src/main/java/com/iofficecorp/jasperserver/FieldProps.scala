package com.iofficecorp.jasperserver

/**
 * Field properties that should be used as keys to refer to
 * field data that could potentially update an xml resource.
 */
sealed trait FieldProps
object FieldProps {
  case object ID extends FieldProps
  case object LABEL extends FieldProps
  case object DATA_TYPE extends FieldProps
  case object ACTIVE extends FieldProps
}
