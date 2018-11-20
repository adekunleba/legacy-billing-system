package com.legacy

import org.scalatest._

class TestBillSpec extends FlatSpec with Matchers {

  val firstPurchase = List("Cola", "Coffee", "Cheese Sandwish")


  val allDrinks = List("Cola", "Coffee", "Cola", "Coffee")

  val hotFoodPurchase = List("Coffee", "Steak Sandwish")

  val foodPurchase = List("Cola", "Coffee", "Cheese Sandwish")


  val moreFoods = allDrinks ::: hotFoodPurchase ::: allDrinks :::
    hotFoodPurchase :::  allDrinks ::: allDrinks ::: hotFoodPurchase :::
    hotFoodPurchase ::: allDrinks ::: allDrinks ::: hotFoodPurchase
       //Drink("Coffee", Pounds(1.00), PurchasesTypes.COLD)

  it should "show give total amount of purchases" in {
    val totalPrice = new ServiceCharge(firstPurchase).makePurchaseBill
    totalPrice shouldEqual 3.5
    totalPrice.isInstanceOf[Double] shouldEqual true
  }

  it should "not apply any service charge when all purchases are drinks" in {
    val serviceCharge = new ServiceCharge(allDrinks)
    val totalPrice = serviceCharge.makePurchaseBill
    val totalAfterServiceCharge = serviceCharge.getBill
    totalAfterServiceCharge shouldEqual totalPrice
  }

  it should "return 10% of service charge" in {
    val serviceCharge = new ServiceCharge(foodPurchase)
    val serviceChargeAmount = serviceCharge.calculateHotnessCharge
    serviceChargeAmount shouldEqual 0.35
  }

  it should "return 20% of service charge" in {
    val serviceCharge = new ServiceCharge(hotFoodPurchase)
    val hotFoodServiceCharge = serviceCharge.calculateHotnessCharge
    hotFoodServiceCharge shouldEqual 1.10
  }

  it should "not add more than 20 pounds to service charge" in {
    val serviceCharge = new ServiceCharge(moreFoods)
    val totalPrice = serviceCharge.makePurchaseBill
    val billDifference = totalPrice - serviceCharge.getBill
    billDifference should be <= 20.0
  }
}