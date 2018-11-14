package com.legacy

import org.scalatest._

class TestBillSpec extends FlatSpec with Matchers {

  val firstPurchase = List(Drink("Cola", Pence(50), PurchasesTypes.COLD))

  val allPurchase :List[Purchases] = firstPurchase :+
    Drink("Coffee", Pounds(1.00), PurchasesTypes.HOT) :+
    Food("Cheese Sandwich", Pounds(2.00), PurchasesTypes.COLD)

  val allDrinks :List[Purchases] = firstPurchase :+
    Drink("Coffee", Pounds(1.00), PurchasesTypes.HOT) :+
    Drink("Coffee", Pounds(1.00), PurchasesTypes.HOT)

  val hotFoodPurchase:List[Purchases] = firstPurchase :+
    Drink("Coffee", Pounds(1.00), PurchasesTypes.HOT) :+
    Food("Cheese Sandwich", Pounds(2.00), PurchasesTypes.HOT)


  val moreFoods = allPurchase ::: allDrinks ::: hotFoodPurchase :::
  allPurchase ::: allDrinks ::: hotFoodPurchase ::: allPurchase ::: allDrinks :::
  allPurchase ::: allDrinks ::: hotFoodPurchase ::: hotFoodPurchase ::: allDrinks :::
  allPurchase ::: allDrinks ::: hotFoodPurchase
       //Drink("Coffee", Pounds(1.00), PurchasesTypes.COLD)

  it should "show give total amount of purchases" in {
    val totalPrice = StandardBill.makePurchaseBill(allPurchase)
    totalPrice shouldEqual 3.5
    totalPrice.isInstanceOf[Double] shouldEqual true
  }

  it should "not apply any service charge when all purchases are drinks" in {
    val totalPrice = StandardBill.makePurchaseBill(allDrinks)
    val serviceCharge = new ServiceCharge(allDrinks, totalPrice)
    val totalAfterServiceCharge = serviceCharge.getBill
    totalAfterServiceCharge shouldEqual totalPrice
  }

  it should "return 10% of service charge" in {
    val totalPrice = StandardBill.makePurchaseBill(allPurchase)
    val serviceCharge = new ServiceCharge(allPurchase, totalPrice)
    val serviceChargeAmount = serviceCharge.getFoodCharge
    serviceChargeAmount shouldEqual 0.35
  }

  it should "return 20% of service charge" in {
    val totalPrice = StandardBill.makePurchaseBill(hotFoodPurchase)
    val serviceCharge = new ServiceCharge(hotFoodPurchase, totalPrice)
    val hotFoodServiceCharge = serviceCharge.calculateHotnessCharge
    hotFoodServiceCharge shouldEqual 0.70
  }

  it should "only add 20 pounds to service charge" in {
    val totalPrice = StandardBill.makePurchaseBill(moreFoods)
    val serviceCharge = new ServiceCharge(moreFoods, totalPrice)
    val billDifference = totalPrice - serviceCharge.getBill
    billDifference should be <= 20.0
  }
}