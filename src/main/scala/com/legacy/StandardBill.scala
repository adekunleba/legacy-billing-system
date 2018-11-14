package com.legacy

object StandardBill {

  import CanConvertImplicits._


  private def makeAmount(money: Money) :Amount = money match {
    case p : Pence => convert(p)
    case q: Pounds => convert(q)
  }

  def makePurchaseBill(purchase:List[Purchases ]):Double ={
    val goodsPrice :List[Amount] = purchase map {
      case d: Food =>
        makeAmount(d.amount)
      case e: Drink =>
        makeAmount(e.amount)
    }
    goodsPrice.map(_.value).sum
  }

}