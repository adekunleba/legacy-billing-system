package com.legacy

import org.scalatest._

class TestBillSpec extends FlatSpec with Matchers {

  val firstPurchase = List("Cola", "Coffee", "Cheese Sandwish")


  val allDrinks = List("Cola", "Coffee", "Cola", "Coffee")

  val hotFoodPurchase = List("Coffee", "Steak Sandwish")

  val foodPurchase = List("Cola", "Coffee", "Cheese Sandwish")

  val oddFoods = List("Cola", "Fish Sandwish", "Coffee")


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

  it should "give total amount of purchases" in {
    val totalPrice = ServiceCharge.bill(firstPurchase)
    totalPrice shouldEqual 3.5
    totalPrice.isInstanceOf[Double] shouldEqual true
  }

  it should "not apply any service charge when all purchases are drinks" in {
    val totalPrice = ServiceCharge.bill(allDrinks)
    val totalAfterServiceCharge = ServiceCharge.billWithCharge(allDrinks)
    totalAfterServiceCharge shouldEqual totalPrice
  }

  it should "return 10% of service charge in case there is FOOD" in {
    val totalAfterServiceCharge = ServiceCharge.billWithCharge(foodPurchase)
    val totalPrice  = ServiceCharge.bill(foodPurchase)
    totalAfterServiceCharge shouldEqual 0.35 + totalPrice
  }

  it should "return 20% of service charge in case there is hot FOOD" in {
    val totalAfterServiceCharge = ServiceCharge.billWithCharge(hotFoodPurchase)
    val totalFoodOnly = ServiceCharge.bill(hotFoodPurchase)
    totalAfterServiceCharge shouldEqual 1.10 + totalFoodOnly
  }

  it should "not add more than 20 pounds to service charge no matter how much items were ordered" in {
    val billWithServiceCharge = ServiceCharge.billWithCharge(moreFoods)
    val totalBillOnly = ServiceCharge.bill(moreFoods)
    val billDifference = billWithServiceCharge - totalBillOnly
    billDifference should be <= 20.0
  }

}