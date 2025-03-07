package loanCalc

import oxygen.predef.core.*

final case class USD private (totalCents: Long) {

  def +(that: USD): USD = USD(this.totalCents + that.totalCents)

  def -(that: USD): USD = USD(this.totalCents - that.totalCents)

  def *(mult: Interest): USD = USD((totalCents * mult.monthly).ceil.toLong)

  def toDouble: Double = totalCents.toDouble / 100

  def dollars: Long = totalCents / 100
  def cents: Long = totalCents % 100

  override def toString: String = s"$$$dollars.${cents.toString.alignRight(2, '0')}"

}
object USD {

  val zero: USD = USD(0L)

  def dollars(amount: Double): USD = USD((amount * 100).ceil.toLong)
  def cents(amount: Long): USD = USD(amount)

  given Ordering[USD] = Ordering.by(_.totalCents)

}

final case class Payment(name: String, amount: USD, startMonth: Int, endMonth: Option[Int]) {
  override def toString: String = s"[$name] $amount : $startMonth -> ${endMonth.fold("EOL")(_.toString)}"
}
object Payment {

  def forever(name: String, amount: USD, startMonth: Int): Payment =
    Payment(name, amount, startMonth, None)

  def startPlusDuration(name: String, amount: USD, startMonth: Int, durationInMonths: Int): Payment =
    Payment(name, amount, startMonth, (startMonth + durationInMonths - 1).some)

}

final case class Interest private (monthly: Double, yearly: Double) {
  override def toString: String = s"${yearly * 100}%"
}
object Interest {
  def monthly(percent: Double): Interest = Interest(percent / 100, percent * 12 / 100)
  def yearly(percent: Double): Interest = Interest(percent / 1200, percent / 100)
}
