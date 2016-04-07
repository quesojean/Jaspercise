package com.iofficecorp.jasperserver

import com.iofficecorp.jasperserver.pumped.report.Field
import com.iofficecorp.jasperserver.pumped.report._
import com.iofficecorp.jasperserver.pumped.report.components.Column
import com.iofficecorp.jasperserver.pumped.report.components._

import scala.util.Try
import scalaxb._
import com.iofficecorp.jasperserver.pumped._
import com.iofficecorp.jasperserver.pumped.domain._
import java.io.InputStream
import scala.xml.{NodeSeq, Elem, NamespaceBinding, XML}

sealed trait FieldProps
object FieldProps {
  case object ID extends FieldProps
  case object LABEL extends FieldProps
  case object DATA_TYPE extends FieldProps
  case object ACTIVE extends FieldProps
}

sealed trait JasperNamespace
object JasperNamespace {
  def DOMAIN: NamespaceBinding = scalaxb.toScope(Some("jsdomain") -> defaultScope.getURI("jsdomain"))
  def REPORT: NamespaceBinding = scalaxb.toScope(Some("jr") -> defaultScope.getURI("jr"))
}

sealed trait Danceable[T] extends Product with Serializable  {
  //self type? to guarantee only works with certain types?
  def ROOT_NODE_LABEL:String
  def scope:NamespaceBinding
  def resourceXML:Elem
  def workIt:Option[String]
  //TODO I feel like workIt between two case classes that implement this trait are VERY similar.  How can
  //we bring implementation up here? (had issue with implicit parameters in scalaxb's fromxml and toxml methods)
  //ERROR : Error:(34, 25) could not find implicit value for parameter format: scalaxb.XMLFormat[T]
  //Error occurred in an application involving default arguments.
  //def root:T= fromXML[T](resourceXML)
  //^
  /*def root:T= fromXML[T](resourceXML)
  def rootCopy:T

  def workIt():Option[String] = {
    if (!root.equals(rootCopy)) {
      val updatedXML: NodeSeq = toXML(rootCopy, None, ROOT_NODE_LABEL, scope)
      Some(updatedXML.toString())
    } else {
      None
    }
  }*/
}

final private case class DomainBuilder(resourceXML: Elem, tableIds:Seq[String], fieldProps: Map[FieldProps, String]) extends Danceable[SchemaType] {
  //look at how we can make this generic for both domain, adhoc, reports
  override val ROOT_NODE_LABEL = "schema"
  override val scope: NamespaceBinding = JasperNamespace.DOMAIN
  val itemId:String = fieldProps(FieldProps.ID)
  val itemLabel:String = fieldProps(FieldProps.LABEL)
  val itemDataType:String = fieldProps(FieldProps.DATA_TYPE)
  val itemActive:Boolean = Try(fieldProps(FieldProps.ACTIVE).toBoolean).getOrElse(false)

  /**
   * Uses fieldId and fieldLabel. (we dont need table prefix since inside group)
   *
   * @param itemDataRecordList
   * @return
   */
  def updateDataRecordItemTypeableList(itemDataRecordList: Seq[DataRecord[ItemTypable]]): Seq
    [DataRecord[ItemTypable]] = {
    itemDataRecordList map {
      case itemToUpdate: DataRecord[ItemTypable] if doUpdateItem(itemToUpdate.value) =>
        val itemType = itemToUpdate.as[ItemType]
        DataRecord(itemToUpdate.namespace,
          itemToUpdate.key,
          itemType.copy(attributes = itemType.attributes + ("@label" -> DataRecord(itemLabel))))
      case item => item
    }
  }

  def getItemGroups(schema: SchemaType): Option[ItemGroupsType] = schema.itemGroups.map {
    //ITEM GROUPS NODE
    case itemGroups: ItemGroupsType => itemGroups
      .copy(itemgroupstypeoption = getItemGroupList(itemGroups))
  }

  def doUpdateItem(item: ItemTypable): Boolean = {
    item.attr_resourceId.endsWith(itemId)
  }

  /**
   * Determine whether this group should be updated based on table names.
   * OR we can grab all groups that match the tables. and case based on tuples.
   *
   * @param itemGroup
   * @return
   */
  def doUpdateItemGroup(itemGroup:ItemGroupType): Boolean = {
    //this is a little loose, not sure about this check
    tableIds.exists(tableId => itemGroup.attr_id.endsWith(tableId))
  }

  def getItemGroupList(itemGroups: ItemGroupsType): Seq[DataRecord[ItemGroupType]] = itemGroups
    .itemgroupstypeoption.map {

    //BUILDING THE GROUP DATA RECORD
    //either copy itemGroup without doing any manipulation, or do something.
    case itemGroupDataRecordToUpdate: DataRecord[ItemGroupType]
      if doUpdateItemGroup(itemGroupDataRecordToUpdate.value) =>
      //ITEM LIST FOR THE ITEM GROUP
      DataRecord(itemGroupDataRecordToUpdate.namespace,
        itemGroupDataRecordToUpdate.key,
        itemGroupDataRecordToUpdate.value.copy(items = getItems(itemGroupDataRecordToUpdate.value)))
    case itemGroup => itemGroup
  }

  def getItems(itemGroup: ItemGroupType): Option[ItemsType] = itemGroup.items.flatMap {
    //ITEM for the list
    case items: ItemsType => Option(items
      .copy(itemstypeoption = updateDataRecordItemTypeableList(items.itemstypeoption)))
  }

  override def workIt: Option[String] = {
    val rootObject: SchemaType = fromXML[SchemaType](resourceXML)
    val rootObjectCopy: SchemaType = rootObject.copy(itemGroups = getItemGroups(rootObject))
    if (!rootObject.equals(rootObjectCopy)) {
      val updatedXML: NodeSeq = toXML(rootObjectCopy, None, ROOT_NODE_LABEL, scope)
      Some(updatedXML.toString())
    } else {
      None
    }
  }

  /*override def rootCopy: SchemaType = {
    root.copy(itemGroups = getItemGroups(root))
  }*/
}

final private case class ReportBuilder(resourceXML: Elem, tableIds:Seq[String], fieldProps: Map[FieldProps, String]) extends Danceable[JasperReport] {
  override val ROOT_NODE_LABEL = "jasperReport"
  override val scope: NamespaceBinding = JasperNamespace.REPORT
  val fieldId = fieldProps(FieldProps.ID)
  val fieldLabel = fieldProps(FieldProps.LABEL)
  val fieldDataType = fieldProps(FieldProps.DATA_TYPE)
  val fieldActive:Boolean = Try(fieldProps(FieldProps.ACTIVE).toBoolean).getOrElse(false)

  def doUpdateField(field: Field): Boolean = {
    field.property.exists(prop => prop.attr_name == "resourceId" && prop.attr_value.get
      .endsWith(fieldId))
  }

  def getFieldProperties(field:Field): Seq[Property] = {
    field.property map {
      case propToUpdate: Property if propToUpdate.attr_name == "adhoc.display" =>
        propToUpdate.copy(attributes = propToUpdate.attributes + ("@value" -> DataRecord(fieldLabel)))
      case prop => prop
    }
  }

  def getFields(report: JasperReport): Seq[Field] = {
    report.field map {
      case updatedField if doUpdateField(updatedField)  =>
        updatedField.copy(property = getFieldProperties(updatedField) )
      case field => field
    }
  }

  /**
   * TODO: REFACTOR THIS CRAY CRAY
   *
   * @param column
   * @return
   */
  def getColumnHeader(column: Column): Option[TableCell] = {
    def textExprMatch(textExpr:String): Boolean = {
      val fieldParts = textExpr.stripPrefix("$F{").stripSuffix("}").split("\\.")
      fieldParts.size == 2 && tableIds.exists(fieldParts.head.endsWith(_) && fieldParts.last.equals(fieldId))
    }
    val doUpdate = column.detailCell.tablecelloption.exists(record => record.key.contains("textField")
      && record.as[TextField].textFieldExpression.exists(textExpr => textExpr.mixed.exists(record
    => textExprMatch(record.as[String]) )))

    if (doUpdate) {
      column.columnHeader.flatMap {
        case tableCellToUpdate: TableCell => Option(tableCellToUpdate.copy(tablecelloption =
          tableCellToUpdate.tablecelloption map {
            case tableCellToUpdate: DataRecord[Any] if tableCellToUpdate.key.contains("textField") =>
              val textField = tableCellToUpdate.as[TextField]
              DataRecord(tableCellToUpdate.namespace, tableCellToUpdate.key, textField.copy(
                textFieldExpression = textField.textFieldExpression flatMap {
                  case textFieldExpr => Option(textFieldExpr.copy(mixed = textFieldExpr.mixed map {
                    case record: DataRecord[Any] => DataRecord("\"" + fieldLabel + "\"")
                  }))
                }))
            case tableCell => tableCell
          }))
      }
    } else {
      column.columnHeader
    }
  }

  def getColumnGroupOptions(columnGroupOptions: Seq[DataRecord[ColumnGroupOption]]): Seq
    [DataRecord[ColumnGroupOption]] = {
    columnGroupOptions map {
      case columnToUpdate: DataRecord[ColumnGroupOption] if columnToUpdate.key.contains("column") =>
        val column = columnToUpdate.as[Column]
        DataRecord(columnToUpdate.namespace, columnToUpdate.key,
          column.copy(columnHeader = getColumnHeader(column)))
      case record => record
    }
  }

  def getTableOptions(tableOptions: Seq[DataRecord[TableOption]]): Seq
    [DataRecord[TableOption]] = {
    tableOptions map {
      case columnGroupToUpdate: DataRecord[TableOption] if columnGroupToUpdate.key.contains("columnGroup") =>
        val columnGroup: com.iofficecorp.jasperserver.pumped.report.components.ColumnGroup = columnGroupToUpdate.as[com.iofficecorp.jasperserver.pumped.report.components.ColumnGroup]
        DataRecord(columnGroupToUpdate.namespace,
          columnGroupToUpdate.key,
          columnGroup.copy(columngroupoption = getColumnGroupOptions(columnGroup.columngroupoption)))
      case tableOption => tableOption
    }
  }

  def getComponent(component: DataRecord[Any]): DataRecord[Any] = {
    component match {
      case tableComp: DataRecord[Any] if tableComp.key.contains("table")  =>
        val table = tableComp.as[Table]
        DataRecord(tableComp.namespace, tableComp.key, table.copy(tableoption = getTableOptions(table.tableoption)))
      case _ => component
    }
  }

  def getBandOptions(bandOptions: Seq[DataRecord[BandOption]]): Seq
    [DataRecord[BandOption]] = {
    bandOptions map {
      case bandOptionToUpdate: DataRecord[BandOption] if bandOptionToUpdate.key.contains("componentElement") =>
        val componentElement: ComponentElement = bandOptionToUpdate.as[ComponentElement]
        DataRecord(bandOptionToUpdate.namespace,
          bandOptionToUpdate.key,
          componentElement.copy(component = getComponent(componentElement.component)))
      case bandOption => bandOption
    }
  }

  def getSummary(report: JasperReport): Option[Summary] = {
    report.summary.flatMap {
      case summary => Option(summary.copy(band = summary.band.flatMap {
        case band => Option (band.copy(bandoption = getBandOptions(band.bandoption)))
      }))
    }
  }

  override def workIt(): Option[String] = {
    val rootObject: JasperReport = fromXML[JasperReport](resourceXML)
    val rootObjectCopy: JasperReport = rootObject.copy(field = getFields(rootObject), summary = getSummary(rootObject))
    if (!rootObject.equals(rootObjectCopy)) {
      val updatedXML: NodeSeq = toXML(rootObjectCopy, None, ROOT_NODE_LABEL, scope)
      Some(updatedXML.toString())
    } else {
      None
    }
  }

  /*override def rootCopy: JasperReport = root.copy(field = getFields(root), summary = getSummary(root))*/
}


//Factory to instantiate correct case class and call appropriate method to return updated xml
object Jaspercise {

  //NOTE if we return the case class, its Product/Serializable type.
  def apply(resourceStream: InputStream, tableIds: Seq[String], fieldProps: Map[FieldProps, String]): Option[String] = {
    val resourceXML: Elem = XML.load(resourceStream)
    val domainNamespaceURI = JasperNamespace.DOMAIN.uri
    val reportNamespaceURI = JasperNamespace.REPORT.uri
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