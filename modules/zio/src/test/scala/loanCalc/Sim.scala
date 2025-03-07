package loanCalc

import oxygen.predef.core.*
import scala.Ordering.Implicits.infixOrderingOps
import scala.annotation.tailrec

object Sim extends scala.App {

  final case class Scenario(
      name: String,
      loan: USD,
      interestRate: Interest,
      buyDown: USD,
      additionalPayments: Seq[Payment],
      termYears: Int,
  ) {

    val termMonths: Int = termYears * 12

    val baseMonthlyPayment: USD = {
      val `(1+I)^N`: Double = Math.pow(1 + interestRate.monthly, termMonths)

      USD.dollars(
        loan.toDouble *
          (interestRate.monthly * `(1+I)^N`) /
          (`(1+I)^N` - 1),
      )
    }

    override def toString: String =
      s"""=====| Scenario : $name |=====
         |loan: $loan
         |interest-rate: $interestRate
         |buy-down: $buyDown
         |base-monthly-payment: $baseMonthlyPayment
         |additional-payments:${additionalPayments.map(p => s"\n    - $p").mkString}
         |num-years:$termYears""".stripMargin

  }
  object Scenario {

    def make(name: String)(loan: USD, interestRate: Interest, buyDown: USD, termYears: Int)(payments: Payment*): Scenario =
      Scenario(
        name = name,
        loan = loan,
        interestRate = interestRate,
        buyDown = buyDown,
        additionalPayments = payments,
        termYears = termYears,
      )

  }

  final case class Month(
      monthNo: Int,
      startingBalance: USD,
      newInterest: USD,
      paid: USD,
      resultingBalance: USD,
  ) {

    def paidToPrinciple: USD = paid - newInterest

    override def toString: String =
      s"$monthNo : $startingBalance + $newInterest - $paid = $resultingBalance"

  }

  final case class Result(
      scenario: Scenario,
      months: List[Month],
  ) {
    def totalPaid: USD = scenario.buyDown + months.map(_.paid).foldLeft(USD.zero)(_ + _)
    def totalPaidInterest: USD = months.map(_.newInterest).foldLeft(USD.zero)(_ + _)
    def totalPaidPrinciple: USD = months.map(_.paidToPrinciple).foldLeft(USD.zero)(_ + _)
  }

  //////////////////////////////////////////////////////////////////////////////////////////////////////
  //      ...
  //////////////////////////////////////////////////////////////////////////////////////////////////////

  def runSim(scenario: Scenario): Result = {
    @tailrec
    def loop(
        monthNo: Int,
        remainingLoan: USD,
        rMonths: List[Month],
    ): Result = {
      val paymentsThisMonth: Seq[Payment] = scenario.additionalPayments.filter { p =>
        p.startMonth <= monthNo &&
        p.endMonth.fold(true)(_ >= monthNo)
      }
      val maxPayThisMonth: USD = scenario.baseMonthlyPayment + paymentsThisMonth.map(_.amount).foldLeft(USD.zero)(_ + _)

      val newInterest: USD = remainingLoan * scenario.interestRate
      val valueAfterInterest: USD = remainingLoan + newInterest
      val paidAmount: USD = valueAfterInterest min maxPayThisMonth
      val endOfMonthValue: USD = valueAfterInterest - paidAmount

      val month = Month(monthNo, remainingLoan, newInterest, paidAmount, endOfMonthValue)
      val newMonths = month :: rMonths

      if (endOfMonthValue == USD.zero) Result(scenario, newMonths.reverse)
      else loop(monthNo + 1, endOfMonthValue, newMonths)
    }

    loop(1, scenario.loan, Nil)
  }

  //////////////////////////////////////////////////////////////////////////////////////////////////////
  //      ...
  //////////////////////////////////////////////////////////////////////////////////////////////////////

  val loanAmount: USD = USD.dollars(584_950 * 0.8)

  val aquinnahPayment: Payment = Payment.forever("aquinnah", USD.dollars(1_000), 1)
  val renter1Year: Payment = Payment.startPlusDuration("renter(1Y)", USD.dollars(1_000), 4, 12)
  val renter2Years: Payment = Payment.startPlusDuration("renter(2Y)", USD.dollars(1_000), 4, 24)

  val buyDowns: Seq[(Interest, USD)] =
    Seq(
      (Interest.yearly(4.85), USD.dollars(0.0)),
    )

  val extraPaymentss: Seq[Seq[Payment]] =
    Seq(
      Seq(),
      Seq(aquinnahPayment),
      Seq(aquinnahPayment, renter1Year),
      Seq(aquinnahPayment, renter2Years),
    )

  val scenarios: Seq[Scenario] = {
    for {
      extraPayments <- extraPaymentss
      (rate, buyDown) <- buyDowns
    } yield Scenario.make(s"kalin${extraPayments.map(p => s" + ${p.name}").mkString} / $rate @ $buyDown")(loanAmount, rate, buyDown, 30)(extraPayments*)

  }

  val results: Seq[Result] =
    scenarios.map(runSim)

  results.foreach { result =>
    println()
    println()
    println()
    println(result.scenario)

    // println()
    // println("--- Monthly Payments ---")
    // println(result.months.mkString("\n"))

    println()
    println("--- Outputs ---")
    val monthsToPay = result.months.length
    println(s"duration: $monthsToPay (${monthsToPay / 12}Y ${monthsToPay % 12}M)")
    println(s"total-paid: ${result.totalPaid}")
    println(s"interest-paid: ${result.totalPaidInterest}")
    println(s"principle-paid: ${result.totalPaidPrinciple}")

  }

}
