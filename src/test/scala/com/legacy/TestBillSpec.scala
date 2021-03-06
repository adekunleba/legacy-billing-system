package com.legacy

import org.scalatest._

class TestBillSpec extends FlatSpec with Matchers {

  val firstPurchase = List("Cola", "Coffee", "Cheese Sandwish")


  val allDrinks = List("Cola", "Coffee", "Cola", "Coffee")

  val hotFoodPurchase = List("Coffee", "Steak Sandwish")

  val foodPurchase = List("Cola", "Coffee", "Cheese Sandwish")

  val oddFoods = List("Cola", "Fish Sandwish", "Coffee")

  val premiumFoods = List("Cola", "Cheese Sandwish", "Lobster")

  val anotherPremiumFoods = List("Cola", "Coffee", "Lobster")


  val moreFoods = allDrinks ::: hotFoodPurchase ::: allDrinks :::
    hotFoodPurchase :::  allDrinks ::: allDrinks ::: hotFoodPurchase :::
    hotFoodPurchase ::: allDrinks ::: allDrinks ::: hotFoodPurchase :::
    allDrinks ::: hotFoodPurchase ::: allDrinks :::
    hotFoodPurchase :::  allDrinks ::: allDrinks ::: hotFoodPurchase :::
    hotFoodPurchase ::: allDrinks ::: allDrinks ::: hotFoodPurchase :::
    allDrinks ::: hotFoodPurchase ::: allDrinks :::
    hotFoodPurchase :::  allDrinks ::: allDrinks ::: hotFoodPurchase :::
    hotFoodPurchase ::: allDrinks ::: allDrinks ::: hotFoodPurchase
       //DRINK("Coffee", Pounds(1.00), PurchasesTypes.COLD)


  val morePremiumFoods = premiumFoods ::: premiumFoods ::: anotherPremiumFoods :::
    anotherPremiumFoods ::: premiumFoods ::: anotherPremiumFoods ::: premiumFoods ::: premiumFoods :::
  premiumFoods ::: premiumFoods ::: anotherPremiumFoods ::: premiumFoods ::: premiumFoods :::
  premiumFoods ::: premiumFoods ::: anotherPremiumFoods ::: anotherPremiumFoods

  it should "give total amount of purchases" in {
    val (_, totalPrice) = ServiceCharge.billWithCharge(firstPurchase)
    totalPrice shouldEqual 3.5
    totalPrice.isInstanceOf[BigDecimal] shouldEqual true
  }

  it should "not apply any service charge when all purchases are drinks" in {
    val (totalAfterServiceCharge, totalPrice) = ServiceCharge.billWithCharge(allDrinks)
    totalAfterServiceCharge shouldEqual totalPrice
  }

  it should "return 10% of service charge in case there is FOOD" in {
    val (totalAfterServiceCharge, totalPrice) = ServiceCharge.billWithCharge(foodPurchase)
    totalAfterServiceCharge shouldEqual 0.35 + totalPrice
  }

  it should "return 20% of service charge in case there is hot FOOD" in {
    val (totalAfterServiceCharge, totalFoodOnly) = ServiceCharge.billWithCharge(hotFoodPurchase)
    totalAfterServiceCharge shouldEqual 1.10 + totalFoodOnly
  }

  it should "not add more than 20 pounds to service charge no matter how much orderItems were ordered" in {
    val (totalAfterServiceCharge, totalBillOnly) = ServiceCharge.billWithCharge(moreFoods)
    val billDifference = totalAfterServiceCharge - totalBillOnly
    billDifference.toDouble should be <= 20.0
  }

  it should "add 25% to the bill in case a premium item is in purchased" in {
    val (totalAfterServiceCharge, totalBill) = ServiceCharge.billWithCharge(premiumFoods)
    totalAfterServiceCharge shouldEqual 6.93 + totalBill
  }

  it should "not add more than 40 pounds to service charge no matter how much premium items were ordered" in {
    val (totalAfterServiceCharge, totalBillOnly) = ServiceCharge.billWithCharge(morePremiumFoods)
    val billDifference = totalAfterServiceCharge - totalBillOnly
    billDifference.toDouble should be <= 40.0
  }
}