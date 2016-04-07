// Generated by <a href="http://scalaxb.org/">scalaxb</a>.
package com.iofficecorp.jasperserver.pumped.domain


/** 

			Copyright (C) 2005-2014 TIBCO Software Inc. All rights reserved.
			http://www.jaspersoft.com.
			Licensed under commercial JasperSoft Subscription License Agreement

		
*/


case class SchemaType(itemGroups: Option[com.iofficecorp.jasperserver.pumped.domain.ItemGroupsType] = None,
  items: Option[com.iofficecorp.jasperserver.pumped.domain.ItemsType] = None,
  resources: Option[com.iofficecorp.jasperserver.pumped.domain.ResourcesType] = None,
  dataSources: Option[com.iofficecorp.jasperserver.pumped.domain.DataSourcesType] = None,
  attributes: Map[String, scalaxb.DataRecord[Any]] = Map()) {
  lazy val attr_version = attributes("@version").as[BigDecimal]
}

      

trait SchemaTypeAll

case class ObjectsType(Items: Option[com.iofficecorp.jasperserver.pumped.domain.ItemsType] = None,
  ItemGroups: Option[com.iofficecorp.jasperserver.pumped.domain.ItemGroupsType] = None)
      

trait ObjectsTypeAll

case class ItemsType(itemstypeoption: Seq[scalaxb.DataRecord[com.iofficecorp.jasperserver.pumped.domain.ItemTypable]] = Nil)
      

trait ItemsTypeOption

trait ItemTypable extends ItemsTypeOption {
  val attr_id: String
  val attr_resourceId: String
  val attr_label: Option[String]
  val attr_description: Option[String]
  val attr_labelId: Option[String]
  val attr_descriptionId: Option[String]
  val attr_dimensionOrMeasure: Option[String]
  val attr_defaultMask: Option[String]
  val attr_defaultAgg: Option[String]
}


case class ItemType(attributes: Map[String, scalaxb.DataRecord[Any]] = Map()) extends ItemTypable {
  lazy val attr_id = attributes("@id").as[String]
  lazy val attr_resourceId = attributes("@resourceId").as[String]
  lazy val attr_label = attributes.get("@label") map { _.as[String] }
  lazy val attr_description = attributes.get("@description") map { _.as[String] }
  lazy val attr_labelId = attributes.get("@labelId") map { _.as[String] }
  lazy val attr_descriptionId = attributes.get("@descriptionId") map { _.as[String] }
  lazy val attr_dimensionOrMeasure = attributes.get("@dimensionOrMeasure") map { _.as[String] }
  lazy val attr_defaultMask = attributes.get("@defaultMask") map { _.as[String] }
  lazy val attr_defaultAgg = attributes.get("@defaultAgg") map { _.as[String] }
}

      


case class ItemGroupsType(itemgroupstypeoption: Seq[scalaxb.DataRecord[com.iofficecorp.jasperserver.pumped.domain.ItemGroupType]] = Nil)
      

trait ItemGroupsTypeOption

case class ItemGroupType(itemGroups: Option[com.iofficecorp.jasperserver.pumped.domain.ItemGroupsType] = None,
  items: Option[com.iofficecorp.jasperserver.pumped.domain.ItemsType] = None,
  attributes: Map[String, scalaxb.DataRecord[Any]] = Map()) extends ItemTypable with ItemGroupsTypeOption {
  lazy val attr_id = attributes("@id").as[String]
  lazy val attr_resourceId = attributes("@resourceId").as[String]
  lazy val attr_label = attributes.get("@label") map { _.as[String] }
  lazy val attr_description = attributes.get("@description") map { _.as[String] }
  lazy val attr_labelId = attributes.get("@labelId") map { _.as[String] }
  lazy val attr_descriptionId = attributes.get("@descriptionId") map { _.as[String] }
  lazy val attr_dimensionOrMeasure = attributes.get("@dimensionOrMeasure") map { _.as[String] }
  lazy val attr_defaultMask = attributes.get("@defaultMask") map { _.as[String] }
  lazy val attr_defaultAgg = attributes.get("@defaultAgg") map { _.as[String] }
}

      

trait ItemGroupTypeAll

case class DataSourcesType(datasourcestypeoption: Seq[scalaxb.DataRecord[com.iofficecorp.jasperserver.pumped.domain.DataSourceType]] = Nil)
      

trait DataSourcesTypeOption

case class DataSourceType(schemaMap: Option[com.iofficecorp.jasperserver.pumped.domain.SchemaMapType] = None,
  attributes: Map[String, scalaxb.DataRecord[Any]] = Map()) extends DataSourcesTypeOption {
  lazy val attr_id = attributes("@id").as[String]
}

      

trait DataSourceTypeAll

case class SchemaMapType(schemamaptypeoption: Seq[scalaxb.DataRecord[com.iofficecorp.jasperserver.pumped.domain.SchemaEntryType]] = Nil)
      

trait SchemaMapTypeOption

case class SchemaEntryType(schemaentrytypeoption: scalaxb.DataRecord[String],
  attributes: Map[String, scalaxb.DataRecord[Any]] = Map()) extends SchemaMapTypeOption {
  lazy val attr_key = attributes("@key").as[String]
}

      

trait SchemaEntryTypeOption

case class ResourcesType(resourcestypeoption: Seq[scalaxb.DataRecord[com.iofficecorp.jasperserver.pumped.domain.ResourcesTypeOption]] = Nil)
      

trait ResourcesTypeOption

case class FieldList(fieldlistoption: Seq[scalaxb.DataRecord[com.iofficecorp.jasperserver.pumped.domain.FieldType]] = Nil)
      

trait FieldListOption

case class GroupList(grouplistoption: Seq[scalaxb.DataRecord[com.iofficecorp.jasperserver.pumped.domain.GroupType]] = Nil)
      

trait GroupListOption

case class AnyDatasetType(fieldList: Option[com.iofficecorp.jasperserver.pumped.domain.FieldList] = None,
  groupList: Option[com.iofficecorp.jasperserver.pumped.domain.GroupList] = None,
  joinOptions: Option[com.iofficecorp.jasperserver.pumped.domain.JoinOptionsType] = None,
  joinInfo: Option[com.iofficecorp.jasperserver.pumped.domain.JoinInfoType] = None,
  joinList: Option[com.iofficecorp.jasperserver.pumped.domain.JoinListType] = None,
  joinedDataSetList: Option[com.iofficecorp.jasperserver.pumped.domain.JoinedDataSetListType] = None,
  tableRefList: Option[com.iofficecorp.jasperserver.pumped.domain.TableRefListType] = None,
  filterString: Option[String] = None,
  query: Option[String] = None,
  source: Option[com.iofficecorp.jasperserver.pumped.domain.ResourcesType] = None,
  attributes: Map[String, scalaxb.DataRecord[Any]] = Map()) extends ResourcesTypeOption {
  lazy val attr_id = attributes("@id").as[String]
  lazy val attr_datasourceId = attributes.get("@datasourceId") map { _.as[String] }
  lazy val attr_tableName = attributes.get("@tableName") map { _.as[String] }
}

      

trait AnyDatasetTypeAll

case class PropertyMap(propertymapoption: Seq[scalaxb.DataRecord[com.iofficecorp.jasperserver.pumped.domain.EntryType]] = Nil)
      

trait PropertyMapOption

case class FieldType(propertyMap: Option[com.iofficecorp.jasperserver.pumped.domain.PropertyMap] = None,
  attributes: Map[String, scalaxb.DataRecord[Any]] = Map()) extends FieldListOption with FieldList3Option {
  lazy val attr_id = attributes("@id").as[String]
  lazy val attr_type = attributes("@type").as[String]
  lazy val attr_dataSetExpression = attributes.get("@dataSetExpression") map { _.as[String] }
  lazy val attr_fieldDBName = attributes.get("@fieldDBName") map { _.as[String] }
}

      

trait FieldTypeAll

case class EntryType(string: Option[String] = None,
  attributes: Map[String, scalaxb.DataRecord[Any]] = Map()) extends PropertyMapOption {
  lazy val attr_key = attributes("@key").as[String]
}

      

trait EntryTypeAll

case class GroupType(attributes: Map[String, scalaxb.DataRecord[Any]] = Map()) extends GroupListOption {
  lazy val attr_columnName = attributes("@columnName").as[String]
  lazy val attr_ascending = attributes.get("@ascending") map { _.as[String] }
}

      


case class FieldList2(fieldlist2option: Seq[scalaxb.DataRecord[com.iofficecorp.jasperserver.pumped.domain.FieldRefType]] = Nil)
      

trait FieldList2Option

case class DataSetRefType(fieldList: Option[com.iofficecorp.jasperserver.pumped.domain.FieldList2] = None,
  joinInfo: Option[com.iofficecorp.jasperserver.pumped.domain.JoinInfoType] = None,
  joinList: Option[com.iofficecorp.jasperserver.pumped.domain.JoinListType] = None,
  joinedDataSetList: Option[com.iofficecorp.jasperserver.pumped.domain.JoinedDataSetListType] = None,
  attributes: Map[String, scalaxb.DataRecord[Any]] = Map()) extends ResourcesTypeOption {
  lazy val attr_id = attributes("@id").as[String]
  lazy val attr_referenceId = attributes("@referenceId").as[String]
  lazy val attr_dataSetType = attributes.get("@dataSetType") map { _.as[String] }
}

      

trait DataSetRefTypeAll

case class JoinInfoType(attributes: Map[String, scalaxb.DataRecord[Any]] = Map()) {
  lazy val attr_alias = attributes("@alias").as[String]
  lazy val attr_referenceId = attributes("@referenceId").as[String]
  lazy val attr_joinType = attributes("@joinType").as[String]
}

      


case class JoinOptionsType(attributes: Map[String, scalaxb.DataRecord[Any]] = Map()) {
  lazy val attr_includeAllDataIslandJoins = attributes("@includeAllDataIslandJoins").as[Boolean]
  lazy val attr_suppressCircularJoins = attributes("@suppressCircularJoins").as[Boolean]
}

      


case class FieldRefType(attributes: Map[String, scalaxb.DataRecord[Any]] = Map()) extends FieldList2Option {
  lazy val attr_id = attributes("@id").as[String]
  lazy val attr_referenceId = attributes("@referenceId").as[String]
}

      


case class JoinListType(joinlisttypeoption: Seq[scalaxb.DataRecord[com.iofficecorp.jasperserver.pumped.domain.JoinType]] = Nil)
      

trait JoinListTypeOption

case class JoinType(attributes: Map[String, scalaxb.DataRecord[Any]] = Map()) extends JoinListTypeOption {
  lazy val attr_left = attributes("@left").as[String]
  lazy val attr_right = attributes("@right").as[String]
  lazy val attr_type = attributes("@type").as[String]
  lazy val attr_expr = attributes("@expr").as[String]
  lazy val attr_weight = attributes("@weight").as[Int]
}

      


case class TableRefListType(tablereflisttypeoption: Seq[scalaxb.DataRecord[com.iofficecorp.jasperserver.pumped.domain.TableRefType]] = Nil)
      

trait TableRefListTypeOption

case class TableRefType(attributes: Map[String, scalaxb.DataRecord[Any]] = Map()) extends TableRefListTypeOption {
  lazy val attr_tableId = attributes("@tableId").as[String]
  lazy val attr_tableAlias = attributes.get("@tableAlias") map { _.as[String] }
  lazy val attr_alwaysIncludeTable = attributes("@alwaysIncludeTable").as[Boolean]
}

      


case class JoinedDataSetListType(joineddatasetlisttypeoption: Seq[scalaxb.DataRecord[com.iofficecorp.jasperserver.pumped.domain.JoinedDataSetRefType]] = Nil)
      

trait JoinedDataSetListTypeOption

case class JoinedDataSetRefType(joineddatasetreftypeoption: scalaxb.DataRecord[String]) extends JoinedDataSetListTypeOption
      

trait JoinedDataSetRefTypeOption

case class FieldList3(fieldlist3option: Seq[scalaxb.DataRecord[com.iofficecorp.jasperserver.pumped.domain.FieldType]] = Nil)
      

trait FieldList3Option

case class JrQueryDataSetType(fieldList: Option[com.iofficecorp.jasperserver.pumped.domain.FieldList3] = None,
  query: Option[String] = None,
  dsReferenceURI: Option[String] = None,
  attributes: Map[String, scalaxb.DataRecord[Any]] = Map()) extends ResourcesTypeOption {
  lazy val attr_id = attributes.get("@id") map { _.as[String] }
  lazy val attr_datasourceId = attributes("@datasourceId").as[String]
  lazy val attr_queryLanguage = attributes.get("@queryLanguage") map { _.as[String] }
  lazy val attr_maxRows = attributes.get("@maxRows") map { _.as[String] }
}

      

trait JrQueryDataSetTypeAll
