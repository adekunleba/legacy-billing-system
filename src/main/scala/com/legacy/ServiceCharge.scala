package com.legacy

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

class ServiceCharge(items: List[String]){
  private def round(value: BigDecimal): Double = value.setScale(2, BigDecimal.RoundingMode.HALF_UP).toDouble
  val totalAmountWithoutServiceCharge: Double = round(makePurchaseBill(items, makeItem, computeItemCost))

  lazy val isHot: List[Purchases] => List[Boolean] = (x: List[Purchases]) => x map {
    case d: Food if d.foodType == HOT => true
    case _ => false
  }
  lazy val makeItem: String => Purchases = (x: String) => { x.toLowerCase match  {
      case "cola" => Drink("cola", 0.50, COLD)
      case "coffee" => Drink("coffee", 1.00, HOT)
      case "cheese sandwish" => Food("cheese sandwish", 2.00,COLD)
      case "steak sandwish" => Food("steaK sandwish", 4.50, HOT)
      case _ => throw new IllegalArgumentException(s"Item $x not found")
    }
  }
  lazy val computeItemCost: List[Purchases] => BigDecimal = (i: List[Purchases]) => i.map(_.price).sum
  def makePurchaseBill(purchaseList: List[String], g:String => Purchases, f:List[Purchases] => BigDecimal) =
    f(purchaseList.map( x => g(x)))

  def getBill :Double =  {
    val purchases = items.map(makeItem)
    purchases match {
      case _ if purchases.forall(x => x.isInstanceOf[Drink]) =>  totalAmountWithoutServiceCharge
      case _ if purchases.exists(x => x.isInstanceOf[Food]) =>
        val serviceCharge = round(calculateHotnessCharge(purchases, isHot))
        if (serviceCharge <= 20) totalAmountWithoutServiceCharge + serviceCharge
        else totalAmountWithoutServiceCharge + 20
    }
  }
  def calculateHotnessCharge(purchases: List[Purchases], f:List[Purchases] => List[Boolean]):BigDecimal =
   if(f(purchases).contains(true)) (20 * totalAmountWithoutServiceCharge) /100
   else (10 * totalAmountWithoutServiceCharge) /100
}

