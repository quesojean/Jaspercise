package com.iofficecorp.jasperserver

import java.io.{BufferedInputStream, FileInputStream, InputStream}


object Main {

  //TEMPORARY
  val resourcesPath = "/Users/caseyyancey/Workspace/Jaspercise/target/classes/"


  //val schemaXML: Elem = XML.loadFile(filePath)
  val schemaStream: InputStream = new BufferedInputStream(new FileInputStream(resourcesPath + "schema.xml"))
  val adhocStream: InputStream = new BufferedInputStream(new FileInputStream(resourcesPath + "adhocJrxml.xml"))
  val reportStream: InputStream = new BufferedInputStream(new FileInputStream(resourcesPath + "mainReportJrxml.xml"))

  val joinId = "JoinTree_1"

  val tableId = "dbo_Users"

  val fieldId:String = "FirstName"

  val resourceId: String = joinId + "." +  tableId + "." + fieldId

  val fieldLabel: String = "POOP First Name"

  val possibleTablesAssocWithMappingCode = Seq("Users", "UserSnapshots", "UserMove", "User")

  //val possibleTablesAssocWithMappingCode = Seq("User", "Users")

  val isFieldActive:Boolean = true

  val fieldDataType:String = "java.lang.String" //(datatype == text on ioffice side)

  def main(args: Array[String]): Unit = {

    /* val testRun = new Jaspercise(schemaStream, possibleTablesAssocWithMappingCode, fieldId, fieldDataType, fieldLabel, isFieldActive)
    val testResult: Option[String] = testRun.workIt()
    println(testResult.getOrElse("NOTHING TO SHOW HERE"))*/

    /*val testRun = new Jaspercise(adhocStream, possibleTablesAssocWithMappingCode, fieldId, fieldDataType, fieldLabel, isFieldActive)
    val testResult: Option[String] = testRun.workIt()
    println(testResult.getOrElse("NOTHING TO SHOW HERE"))*/
    import FieldProps._
    val test: ID.type = FieldProps.ID
    val fieldProps: Map[FieldProps, String] = Map(FieldProps.ID -> fieldId, FieldProps.LABEL -> fieldLabel,
        FieldProps.DATA_TYPE-> fieldDataType, FieldProps.ACTIVE -> isFieldActive.toString)
    val testResult: Option[String] = Jaspercise(reportStream, possibleTablesAssocWithMappingCode, fieldProps)
    //val testResult: Option[String] = Jaspercise(schemaStream, possibleTablesAssocWithMappingCode, fieldProps)
    println(testResult.getOrElse("NOTHING TO SHOW HERE"))




    //lets do some scalaxb stuff
    /*val schema: SchemaType = fromXML[SchemaType](

      schemaXML)

    val testScope = defaultScope
    println("TEST SCOPE " + testScope.getURI("jsdomain"))

    val merged: NamespaceBinding = scalaxb
      .toScope(((Some("jsdomain") -> "http://www.jaspersoft.com/2007/SL/XMLSchema") ::
        scalaxb.fromScope(defaultScope)).distinct: _*)
    /*val itemGroups: Option[Elem] = schema.itemGroups.map {
    toXML(_, Some("http://www.jaspersoft.com/2007/SL/XMLSchema"), Some("schema"), merged) match {
    case elem: Elem if (elem.label == "item") && (elem \ "@resourceId").text == resourceId =>
    elem
    .copy(attributes = elem.attributes append Attribute(None,
    "label",
    Text("YOUAREASTAR"),
    scala.xml.Null))
    case x => error("unexpected non-elem: " + x.toString)
    }
    }
    println("BOOTY: " + itemGroups.toString)
    */
    //////////// ROUND 2 ///////////

    def getAndUpdateItemDataRecord(item: ItemType): DataRecord[Any] = {
      item match {
        case e: ItemType if e.attr_id == resourceId =>
          DataRecord[ItemType](None,
            Some("item"),
            e.copy(attributes = e.attributes + ("@label" -> DataRecord("POOP"))))
      }
    }

    val test: ItemsType = schema.itemGroups.head
      .itemgroupstypeoption.head.value.items.get

    /*val addressBook = fromXML[AddressBook](<addressBook
    xmlns="http://www.example.com/address">
    <card><name>foo</name><email>foo@example.com</email></card>
    <card><name>bar</name><email>bar@example.com</email></card>
    </addressBook>)

    val modified = AddressBook(addressBook.card map { card =>
    card.copy(name = card.name + "2")}: _*)
    println(toXML[AddressBook](modified, Some("http://www.example.com/address"),
    Some("addressBook"), defaultScope))*/

    // GET FIRST ITEM GROUP FROM GROUP LIST
    val itemGroup: ItemGroupType = schema.itemGroups.get
      .itemgroupstypeoption.head.value
    /// COPY ONE GROUP
    val itemGroupCopy: ItemGroupType = itemGroup.copy(items = itemGroup.items)

    //GET FIRST ITEM TYPE FROM ITEM LIST (ItemsType-->DataRecord[ItemType]*)
    val firstItem: ItemType = itemGroup.items.get.itemstypeoption.head.value.asInstanceOf[ItemType]
    // COPY ONE ITEM
    val firstItemCopy: ItemType = firstItem
      .copy(attributes = firstItem.attributes + ("@id" -> DataRecord("POOP")))
    val itemDataRecord = DataRecord(None, Some("item"), firstItemCopy)
    val unApplyTest: Map[String, DataRecord[Any]] = ItemType.unapply(firstItemCopy).get

    //Seq.concat(scalaxb.toXML[String](__obj.name, None, Some("name"), __scope, false)
    /*val test2: Option[Seq[Option[Seq[Unit]]]] = schema.itemGroups map {
    case x => x.itemgroupstypeoption map {
    case y => y.value.items map {
    z => z.itemstypeoption map {
    a => println(a.toString)
    }
    }
    }
    }*/


    val list: Seq[DataRecord[ItemTypable]] = schema.itemGroups.map(_.itemgroupstypeoption).get
      .flatMap(_.value.items)
      .flatMap(_.itemstypeoption)
    //.map(_.as[ItemType])


    /*val updatedDataRecordItemTypableList: Seq[DataRecord[ItemTypable]] = list map {
    case itemToUpdate: DataRecord[ItemTypable] if itemToUpdate.value.resourceId == resourceId =>
    val itemType = itemToUpdate.as[ItemType]
    DataRecord(itemToUpdate.namespace, itemToUpdate.key, itemType.copy(attributes = itemType.attributes + ("@label"->DataRecord("POOP"))))
    case item => item
    }*/
    //NOTE ITEMGROUPTYPE AND ITEMTYPE inherit from same trait. CLUE????

    //val list2: Seq[DataRecord[ItemGroupType]] = schema.itemGroups.map(_.itemgroupstypeoption).get


    // HERE HERE HERE HERE
    def updateDataRecordItemTypeableList(itemDataRecordList: Seq[DataRecord[ItemTypable]]): Seq
      [DataRecord[ItemTypable]] = {
      itemDataRecordList map {
        case itemToUpdate: DataRecord[ItemTypable] if itemToUpdate.value.attr_resourceId == resourceId =>
          val itemType = itemToUpdate.as[ItemType]
          DataRecord(itemToUpdate.namespace,
            itemToUpdate.key,
            itemType.copy(attributes = itemType.attributes + ("@label" -> DataRecord("POOP"))))
        case item => item
      }
    }


    //HERE HERE HERE HERE
    val itemGroupsCopy: Option[ItemGroupsType] = schema.itemGroups map {
      case itemGroups: ItemGroupsType => {

        val itemGroupList: Seq[DataRecord[ItemGroupType]] = itemGroups.itemgroupstypeoption.map {
          case itemGroupDataRecord: DataRecord[ItemGroupType] => {
            //we have to rebuild Seq[DataRecord[ItemGroupType]]
            //so must return DataRecord[ItemGroupType]

            //items list
            val itemsCopy = itemGroupDataRecord.value.items.flatMap {
              case items: ItemsType => Option(items
                .copy(itemstypeoption = updateDataRecordItemTypeableList(items.itemstypeoption)))
            }

            //item group and data record
            val itemGroupCopy: ItemGroupType = itemGroupDataRecord.value
              .copy(items = itemsCopy) //PUT itemsUpdatedCopy here
            //create new Item group data record.
            DataRecord(itemGroupDataRecord.namespace, itemGroupDataRecord.key, itemGroupCopy)
          }
          case itemGroup => itemGroup
        }

        itemGroups.copy(itemgroupstypeoption = itemGroupList)
      }
    }

    //THEN WE CAN DO THIS
    //def getItemGroups(), getItemGroup, getItems, getItem
    //like so:
    //schema.copy(itemGroups = getItemGroups)
    //getItemGroups = call
    val schemaCopy = schema.copy(itemGroups = itemGroupsCopy)

    val updatedSchemaXML = toXML(schemaCopy, None, Some("schema"), defaultScope)
    //println("TEST: " + updatedSchemaXML)


    //THE WINNER!!!!
    val schemaCopy2 = schema.copy(itemGroups = schema.itemGroups map {
      //ITEM GROUPS NODE
      case itemGroups: ItemGroupsType =>
        //LIST OF DATA RECORDS OF GROUP TYPE
        itemGroups.copy(itemgroupstypeoption = itemGroups.itemgroupstypeoption.map {
          //BUILDING THE GROUP DATA REOCRD
          case itemGroupDataRecord: DataRecord[ItemGroupType] =>
            //ITEM LIST FOR THE ITEM GROUP
            DataRecord(itemGroupDataRecord.namespace,
              itemGroupDataRecord.key,
              itemGroupDataRecord.value.copy(items = itemGroupDataRecord.value.items.flatMap {
                //ITEM for the list
                case items: ItemsType => Option(items
                  .copy(itemstypeoption = updateDataRecordItemTypeableList(items.itemstypeoption)))
              }))
        })
    })

    val updatedSchemaXML2 = toXML(schemaCopy2, None, Some("schema"), defaultScope)
    println("SLICK: " + updatedSchemaXML)

    def getItemGroups(schema: SchemaType): Option[ItemGroupsType] = schema.itemGroups.map {
      //ITEM GROUPS NODE
      case itemGroups: ItemGroupsType => itemGroups
        .copy(itemgroupstypeoption = getItemGroupList(itemGroups))
    }

    def getItemGroupList(itemGroups: ItemGroupsType): Seq[DataRecord[ItemGroupType]] = itemGroups
      .itemgroupstypeoption.map {
      //BUILDING THE GROUP DATA RECORD
      case itemGroupDataRecord: DataRecord[ItemGroupType] =>
        //ITEM LIST FOR THE ITEM GROUP
        DataRecord(itemGroupDataRecord.namespace,
          itemGroupDataRecord.key,
          itemGroupDataRecord.value.copy(items = getItems(itemGroupDataRecord.value)))
    }

    def getItems(itemGroup: ItemGroupType): Option[ItemsType] = itemGroup.items.flatMap {
      //ITEM for the list
      case items: ItemsType => Option(items
        .copy(itemstypeoption = updateDataRecordItemTypeableList(items.itemstypeoption)))
    }

    def getSchema: SchemaType = schema.copy(itemGroups = getItemGroups(schema))

    //getSchema _ andThen _ getItemGroups


    //andThen/Copy ??

    def f(s: String): String = "f(" + s + ")"
    def g(s: String): String = "g(" + s + ")"
    val fAndThenG = f _ andThen g
    println("BOO " + fAndThenG("blah"))




    //val itemListCopy = ItemsType(itemstypeoption = updatedDataRecordItemTypableList)
    //schema.itemGroups.get.copy(itemgroupstypeoption =  updatedDataRecordItemTypableList)


    //val itemsListXML = toXML(itemListCopy, None, Some("items"), defaultScope)
    //println("itemsListxml " + itemsListXML.toString())

    /*val list2: Seq[DataRecord[ItemTypable]] = schema.itemGroups.map(_.itemgroupstypeoption).get
    .flatMap(_.value.items)
    .flatMap(_.itemstypeoption)


    val itemDataRecordList: Seq[DataRecord[ItemType]] = null

    val updatedList = list2 map {
    case itemDataRecord: DataRecord[ItemType] if itemDataRecord.value.resourceId == resourceId =>  itemDataRecord
    }*/

    // println("UPDATED LIST: " + updatedItemList.size)


    //Seq[DataRecord[ItemTypable]] if (item. == resourceId)  =>  DataRecord[ItemType](None, Some("item"), e.copy( attributes = e.attributes + ("@id"->DataRecord("POOP"))))

    */

  }
}
