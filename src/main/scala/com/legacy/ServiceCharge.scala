package com.legacy

//Using Case Class in case of Enumeration

sealed trait ServedStatus
case object HOT extends ServedStatus
case object COLD extends ServedStatus

trait Purchases {val price: BigDecimal}
case class Drink(name: String, amount : BigDecimal,  itemType: ServedStatus) extends Purchases {
  override val price: BigDecimal = amount
}
case class Food(name: String, amount: BigDecimal, foodType:ServedStatus) extends Purchases {
  override val price: BigDecimal = amount
}


class ServiceCharge(items: List[String]) {
  private val purchaseItems = items.map(makeItem)
  private def round(value: BigDecimal): Double = value.setScale(2, BigDecimal.RoundingMode.HALF_UP).toDouble
  val totalAmount: Double = makePurchaseBill

  private def makeItem(item: String) : Purchases = {
    item.toLowerCase match {
      case "cola" => Drink("cola", 0.50, COLD)
      case "coffee" => Drink("coffee", 1.00, HOT)
      case "cheese sandwish" => Food("cheese sandwish", 2.00,COLD)
      case "steak sandwish" => Food("steaK sandwish", 4.50, HOT)
      case _ => throw new IllegalArgumentException(s"Item $item not found")
    }
  }

  def makePurchaseBill :Double ={
    round(purchaseItems.map(_.price).sum)
  }

  def getFoodCharge :Double =  items match {
    case _ if purchaseItems.forall(x => x.isInstanceOf[Drink]) =>  0
    case _ if purchaseItems.exists(x => x.isInstanceOf[Food]) => calculateHotnessCharge
  }

  def getHotnessCharge: List[Boolean] = items.map(makeItem).map {
      case d: Food if d.foodType == HOT => true
      case _ => false
    }

  def calculateHotnessCharge :Double =
    if (getHotnessCharge.contains(true)) {
       (20 * totalAmount) / 100
    } else (10 * totalAmount) / 100

  def getBill :Double = {
    val totalServiceCharge = getFoodCharge
    if (totalServiceCharge <= 20) totalAmount + totalServiceCharge else totalAmount + 20
  }
}

