package com.legacy

object PurchasesTypes extends Enumeration{
  val HOT, COLD = Value
}

trait Purchases
case class Drink(name: String, amount : Money,  itemType: PurchasesTypes.Value) extends Purchases
case class Food(name: String, amount: Money, foodType:PurchasesTypes.Value) extends Purchases

class ServiceCharge(items: List[Purchases], totalAmount:Double) {
  def getFoodCharge :Double =  items match {
    case _ if items.forall(x => x.isInstanceOf[Drink]) =>  0
    case _ if items.exists(x => x.isInstanceOf[Food]) => (10 * totalAmount) / 100
  }

  def getHotnessCharge: List[Boolean] = items.map {
      case d: Food if d.foodType == PurchasesTypes.HOT => true
      case _ => false
    }

  def calculateHotnessCharge :Double =
    if (getHotnessCharge.contains(true)) {
       (20 * totalAmount) / 100
    } else 0



  def getBill :Double = {
    val totalServiceCharge = calculateHotnessCharge + getFoodCharge
    if (totalServiceCharge <= 20) totalAmount + totalServiceCharge else totalAmount + 20
  }
}

