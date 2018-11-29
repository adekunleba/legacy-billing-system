package com.legacy

sealed trait ServedStatus
case object HOT extends ServedStatus
case object COLD extends ServedStatus

sealed trait ItemType
case object DRINK extends ItemType
case object FOOD extends ItemType

case class MenuItem(name: String, servedStatus: ServedStatus, amount: BigDecimal, itemType: ItemType)

object ServiceCharge{
  private def  purchaseItems(orderItems: List[String]) :List[MenuItem] =
    menuItems.filterKeys(orderItems.map(_.toLowerCase).contains).values.toList

  lazy val menuItems = Map("cola" -> MenuItem("cola", COLD , 0.50, DRINK),
    "coffee" -> MenuItem("coffee", HOT, 1.00, DRINK),
    "cheese sandwish" -> MenuItem("cheese sandwish", COLD, 2.00, FOOD),
    "steak sandwish" -> MenuItem("steak sandwish", HOT, 4.50, FOOD))

  def billWithCharge(orderItems: List[String]) : (BigDecimal, BigDecimal) = {
    val totalBill =  purchaseItems(orderItems).map(_.amount).sum.setScale(2, BigDecimal.RoundingMode.HALF_UP)
    val purchased = purchaseItems(orderItems)
    val serviceCharge :BigDecimal = purchased match {
      case _ if purchased.exists(x => x.itemType ==FOOD && x.servedStatus ==HOT) =>  (20 * totalBill / 100).min(20)
      case _ if purchased.exists(x => x.itemType == FOOD) =>  (10 * totalBill / 100).min(10)
      case _ => 0
    }
    ((totalBill + serviceCharge).setScale(2, BigDecimal.RoundingMode.HALF_UP), totalBill)
  }
}

