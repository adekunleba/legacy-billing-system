package com.legacy

sealed trait ServedStatus
case object HOT extends ServedStatus
case object COLD extends ServedStatus

sealed trait ItemType
case object DRINK extends ItemType
case object FOOD extends ItemType

case class MenuItem(name: String, servedStatus: ServedStatus, amount: BigDecimal, itemType: ItemType)

object ServiceCharge{
  private def  purchaseItems(items: List[String]) :List[MenuItem] =
    myItems.filterKeys(items.map(_.toLowerCase).contains).values.toList

  def  bill(items: List[String]): Double = {
    purchaseItems(items).map(_.amount).sum.setScale(2, BigDecimal.RoundingMode.HALF_UP).toDouble
  }

  lazy val myItems = Map("cola" -> MenuItem("cola", COLD , 0.50, DRINK),
    "coffee" -> MenuItem("coffee", HOT, 1.00, DRINK),
    "cheese sandwish" -> MenuItem("cheese sandwish", COLD, 2.00, FOOD),
    "steak sandwish" -> MenuItem("steak sandwish", HOT, 4.50, FOOD))

  def billWithCharge(items: List[String]) :Double = {
    val thisBill = bill(items)
    val purchased = purchaseItems(items)
    purchased match {
      case _ if purchased.exists(x => x.itemType ==FOOD && x.servedStatus ==HOT) => thisBill + (20 * thisBill / 100).min(20)
      case _ if purchased.exists(x => x.itemType == FOOD) => thisBill + (10 * thisBill / 100).min(20)
      case _ => thisBill
    }
  }
}

