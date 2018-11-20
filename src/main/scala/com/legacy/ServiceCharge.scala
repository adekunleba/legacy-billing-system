package com.legacy

object PurchasesTypes extends Enumeration{
  val HOT, COLD = Value
}

trait Purchases {
  val price: BigDecimal
}
case class Drink(name: String, amount : BigDecimal,  itemType: PurchasesTypes.Value) extends Purchases {
  override val price: BigDecimal = amount
}
case class Food(name: String, amount: BigDecimal, foodType:PurchasesTypes.Value) extends Purchases {
  override val price: BigDecimal = amount
}



class ServiceCharge(items: List[String]) {

  val totalAmount: Double = makePurchaseBill
  private def round(value: BigDecimal): Double = value.setScale(2, BigDecimal.RoundingMode.HALF_UP).toDouble

  def makeItem(item: String) : Purchases = {
    item.toLowerCase match {
      case "cola" => Drink("cola", 0.50, PurchasesTypes.COLD)
      case "coffee" => Drink("coffee", 1.00, PurchasesTypes.HOT)
      case "cheese sandwish" => Food("cheese sandwish", 2.00, PurchasesTypes.COLD)
      case "steak sandwish" => Food("stead sandwish", 4.50, PurchasesTypes.HOT)
      case _ => throw new IllegalArgumentException(s"Item $item not found")
    }
  }

  def makePurchaseBill :Double ={
    round(items.map(makeItem).map(_.price).sum)
  }

  def getFoodCharge :Double =  items match {
    case _ if items.map(makeItem).forall(x => x.isInstanceOf[Drink]) =>  0
    case _ if items.map(makeItem).exists(x => x.isInstanceOf[Food]) => calculateHotnessCharge
  }

  def getHotnessCharge: List[Boolean] = items.map(makeItem).map {
      case d: Food if d.foodType == PurchasesTypes.HOT => true
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

