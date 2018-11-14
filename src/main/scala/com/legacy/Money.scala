package com.legacy

sealed trait Money
case class Pounds(value: Double) extends Money
case class Pence(value: Int) extends Money
case class Amount(value: Double) extends Money



trait Convert[A, B] {
  def convert(a: A) :B
}

//CanAdd is supposed to add any Amount to either Pence or Pounds
object CanConvertImplicits{
  def convert[A, B, C](a: A)(implicit  ev: Convert[A, B]) :B = ev.convert(a)

  implicit object CanConvertPounds extends Convert[Pounds, Amount] {
    override def convert(a: Pounds): Amount = Amount(a.value)
  }

  implicit object CanConvertPence extends Convert[Pence, Amount] {
    override def convert(a: Pence): Amount = Amount(a.value.toDouble / 100)
  }

}

