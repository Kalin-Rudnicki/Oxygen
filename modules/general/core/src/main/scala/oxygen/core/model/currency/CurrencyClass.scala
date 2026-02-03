package oxygen.core.model.currency

import oxygen.core.Text
import oxygen.core.typeclass.StringCodec

sealed abstract class CurrencyClass(final val currencyCode: CurrencyCode) {

  final opaque type Precise = PreciseMoney
  object Precise {

    def apply(value: MoneyValue, roundMode: RoundMode = RoundMode.Error): Precise = PreciseMoney(value, currencyCode, roundMode)

    def unsafeWrap(value: PreciseMoney): Precise =
      if value.currencyCode == currencyCode then value
      else throw new RuntimeException(s"Unable to wrap currency ${value.currencyCode} in $currencyCode")

    extension (self: Precise) {

      def unwrap: PreciseMoney = self

      def unprecise: Unprecise = self.unprecise

      def unary_- : Precise = -self

      def add(by: MoneyValue, roundMode: RoundMode): Precise = self.add(by, roundMode)
      def add(by: MoneyValue): Precise = self.add(by)
      def add(by: Precise): Precise = self.add(by.toBigDecimal)

      def subtract(by: MoneyValue, roundMode: RoundMode): Precise = self.subtract(by, roundMode)
      def subtract(by: MoneyValue): Precise = self.subtract(by)
      def subtract(by: Precise): Precise = self.subtract(by.toBigDecimal)

      def times(by: BigDecimal, roundMode: RoundMode): Precise = self.times(by, roundMode)
      def times(by: Double, roundMode: RoundMode): Precise = self.times(by, roundMode)
      def times(by: Long, roundMode: RoundMode): Precise = self.times(by, roundMode)
      def times(by: BigDecimal): Precise = self.times(by)
      def times(by: Double): Precise = self.times(by)
      def times(by: Long): Precise = self.times(by)

      def divide(by: BigDecimal, roundMode: RoundMode): Precise = self.divide(by, roundMode)
      def divide(by: Double, roundMode: RoundMode): Precise = self.divide(by, roundMode)
      def divide(by: Long, roundMode: RoundMode): Precise = self.divide(by, roundMode)
      def divide(by: BigDecimal): Precise = self.divide(by)
      def divide(by: Double): Precise = self.divide(by)
      def divide(by: Long): Precise = self.divide(by)

      def +(by: MoneyValue): Precise = self.add(by)
      def +(by: Precise): Precise = self.add(by.toBigDecimal)
      def -(by: MoneyValue): Precise = self.subtract(by)
      def -(by: Precise): Precise = self.subtract(by.toBigDecimal)
      def *(by: BigDecimal): Precise = self.times(by)
      def *(by: Double): Precise = self.times(by)
      def *(by: Long): Precise = self.times(by)
      def /(by: BigDecimal): Precise = self.divide(by)
      def /(by: Double): Precise = self.divide(by)
      def /(by: Long): Precise = self.divide(by)

      def show(includeCurrency: Boolean, forceDecimal: Boolean): Text = self.show(includeCurrency, forceDecimal)
      def show: Text = self.show

    }

    given stringCodec: StringCodec[Precise] = Unprecise.stringCodec.transformCatchFail(_.precise(), _.unprecise)

  }

  final opaque type Unprecise = UnpreciseMoney
  object Unprecise {

    def apply(value: MoneyValue): Unprecise = UnpreciseMoney(value, currencyCode)

    def unsafeWrap(value: UnpreciseMoney): Unprecise =
      if value.currencyCode == currencyCode then value
      else throw new RuntimeException(s"Unable to wrap currency ${value.currencyCode} in $currencyCode")

    extension (self: Unprecise) {

      def unwrap: UnpreciseMoney = self

      def precise(roundMode: RoundMode = RoundMode.Error): Precise = self.precise(roundMode)

      def unary_- : Unprecise = -self

      def add(by: MoneyValue): Unprecise = self.add(by)
      def add(by: Unprecise): Unprecise = self.add(by.value)

      def subtract(by: MoneyValue): Unprecise = self.subtract(by)
      def subtract(by: Unprecise): Unprecise = self.subtract(by.value)

      def times(by: BigDecimal): Unprecise = self.times(by)
      def times(by: Double): Unprecise = self.times(by)
      def times(by: Long): Unprecise = self.times(by)

      def divide(by: BigDecimal): Unprecise = self.divide(by)
      def divide(by: Double): Unprecise = self.divide(by)
      def divide(by: Long): Unprecise = self.divide(by)

      def +(by: MoneyValue): Unprecise = self.add(by)
      def +(by: Unprecise): Unprecise = self.add(by.value)
      def -(by: MoneyValue): Unprecise = self.subtract(by)
      def -(by: Unprecise): Unprecise = self.subtract(by.value)
      def *(by: BigDecimal): Unprecise = self.times(by)
      def *(by: Double): Unprecise = self.times(by)
      def *(by: Long): Unprecise = self.times(by)
      def /(by: BigDecimal): Unprecise = self.divide(by)
      def /(by: Double): Unprecise = self.divide(by)
      def /(by: Long): Unprecise = self.divide(by)

      def show(includeCurrency: Boolean, forceDecimal: Boolean): Text = self.show(includeCurrency, forceDecimal)
      def show: Text = self.show

    }

    given stringCodec: StringCodec[Unprecise] = StringCodec.string.transformCatchFail(str => Unprecise(BigDecimal(str)), _.value.toString)

  }

  def apply(value: MoneyValue, roundMode: RoundMode = RoundMode.Error): Precise = PreciseMoney(value, currencyCode, roundMode)

}

type AED = AED.Precise
object AED extends CurrencyClass(CurrencyCode.AED)

type AFN = AFN.Precise
object AFN extends CurrencyClass(CurrencyCode.AFN)

type ALL = ALL.Precise
object ALL extends CurrencyClass(CurrencyCode.ALL)

type AMD = AMD.Precise
object AMD extends CurrencyClass(CurrencyCode.AMD)

type ANG = ANG.Precise
object ANG extends CurrencyClass(CurrencyCode.ANG)

type AOA = AOA.Precise
object AOA extends CurrencyClass(CurrencyCode.AOA)

type ARS = ARS.Precise
object ARS extends CurrencyClass(CurrencyCode.ARS)

type AUD = AUD.Precise
object AUD extends CurrencyClass(CurrencyCode.AUD)

type AWG = AWG.Precise
object AWG extends CurrencyClass(CurrencyCode.AWG)

type AZN = AZN.Precise
object AZN extends CurrencyClass(CurrencyCode.AZN)

type BAM = BAM.Precise
object BAM extends CurrencyClass(CurrencyCode.BAM)

type BBD = BBD.Precise
object BBD extends CurrencyClass(CurrencyCode.BBD)

type BDT = BDT.Precise
object BDT extends CurrencyClass(CurrencyCode.BDT)

type BGN = BGN.Precise
object BGN extends CurrencyClass(CurrencyCode.BGN)

type BHD = BHD.Precise
object BHD extends CurrencyClass(CurrencyCode.BHD)

type BIF = BIF.Precise
object BIF extends CurrencyClass(CurrencyCode.BIF)

type BMD = BMD.Precise
object BMD extends CurrencyClass(CurrencyCode.BMD)

type BND = BND.Precise
object BND extends CurrencyClass(CurrencyCode.BND)

type BOB = BOB.Precise
object BOB extends CurrencyClass(CurrencyCode.BOB)

type BRL = BRL.Precise
object BRL extends CurrencyClass(CurrencyCode.BRL)

type BSD = BSD.Precise
object BSD extends CurrencyClass(CurrencyCode.BSD)

type BTN = BTN.Precise
object BTN extends CurrencyClass(CurrencyCode.BTN)

type BWP = BWP.Precise
object BWP extends CurrencyClass(CurrencyCode.BWP)

type BYN = BYN.Precise
object BYN extends CurrencyClass(CurrencyCode.BYN)

type BZD = BZD.Precise
object BZD extends CurrencyClass(CurrencyCode.BZD)

type CAD = CAD.Precise
object CAD extends CurrencyClass(CurrencyCode.CAD)

type CDF = CDF.Precise
object CDF extends CurrencyClass(CurrencyCode.CDF)

type CHF = CHF.Precise
object CHF extends CurrencyClass(CurrencyCode.CHF)

type CLP = CLP.Precise
object CLP extends CurrencyClass(CurrencyCode.CLP)

type CNY = CNY.Precise
object CNY extends CurrencyClass(CurrencyCode.CNY)

type COP = COP.Precise
object COP extends CurrencyClass(CurrencyCode.COP)

type CRC = CRC.Precise
object CRC extends CurrencyClass(CurrencyCode.CRC)

type CUP = CUP.Precise
object CUP extends CurrencyClass(CurrencyCode.CUP)

type CVE = CVE.Precise
object CVE extends CurrencyClass(CurrencyCode.CVE)

type CZK = CZK.Precise
object CZK extends CurrencyClass(CurrencyCode.CZK)

type DJF = DJF.Precise
object DJF extends CurrencyClass(CurrencyCode.DJF)

type DKK = DKK.Precise
object DKK extends CurrencyClass(CurrencyCode.DKK)

type DOP = DOP.Precise
object DOP extends CurrencyClass(CurrencyCode.DOP)

type DZD = DZD.Precise
object DZD extends CurrencyClass(CurrencyCode.DZD)

type EGP = EGP.Precise
object EGP extends CurrencyClass(CurrencyCode.EGP)

type ERN = ERN.Precise
object ERN extends CurrencyClass(CurrencyCode.ERN)

type ETB = ETB.Precise
object ETB extends CurrencyClass(CurrencyCode.ETB)

type EUR = EUR.Precise
object EUR extends CurrencyClass(CurrencyCode.EUR)

type FJD = FJD.Precise
object FJD extends CurrencyClass(CurrencyCode.FJD)

type FKP = FKP.Precise
object FKP extends CurrencyClass(CurrencyCode.FKP)

type GBP = GBP.Precise
object GBP extends CurrencyClass(CurrencyCode.GBP)

type GEL = GEL.Precise
object GEL extends CurrencyClass(CurrencyCode.GEL)

type GHS = GHS.Precise
object GHS extends CurrencyClass(CurrencyCode.GHS)

type GIP = GIP.Precise
object GIP extends CurrencyClass(CurrencyCode.GIP)

type GMD = GMD.Precise
object GMD extends CurrencyClass(CurrencyCode.GMD)

type GNF = GNF.Precise
object GNF extends CurrencyClass(CurrencyCode.GNF)

type GTQ = GTQ.Precise
object GTQ extends CurrencyClass(CurrencyCode.GTQ)

type GYD = GYD.Precise
object GYD extends CurrencyClass(CurrencyCode.GYD)

type HKD = HKD.Precise
object HKD extends CurrencyClass(CurrencyCode.HKD)

type HNL = HNL.Precise
object HNL extends CurrencyClass(CurrencyCode.HNL)

type HTG = HTG.Precise
object HTG extends CurrencyClass(CurrencyCode.HTG)

type HUF = HUF.Precise
object HUF extends CurrencyClass(CurrencyCode.HUF)

type IDR = IDR.Precise
object IDR extends CurrencyClass(CurrencyCode.IDR)

type ILS = ILS.Precise
object ILS extends CurrencyClass(CurrencyCode.ILS)

type INR = INR.Precise
object INR extends CurrencyClass(CurrencyCode.INR)

type IQD = IQD.Precise
object IQD extends CurrencyClass(CurrencyCode.IQD)

type IRR = IRR.Precise
object IRR extends CurrencyClass(CurrencyCode.IRR)

type ISK = ISK.Precise
object ISK extends CurrencyClass(CurrencyCode.ISK)

type JMD = JMD.Precise
object JMD extends CurrencyClass(CurrencyCode.JMD)

type JOD = JOD.Precise
object JOD extends CurrencyClass(CurrencyCode.JOD)

type JPY = JPY.Precise
object JPY extends CurrencyClass(CurrencyCode.JPY)

type KES = KES.Precise
object KES extends CurrencyClass(CurrencyCode.KES)

type KGS = KGS.Precise
object KGS extends CurrencyClass(CurrencyCode.KGS)

type KHR = KHR.Precise
object KHR extends CurrencyClass(CurrencyCode.KHR)

type KMF = KMF.Precise
object KMF extends CurrencyClass(CurrencyCode.KMF)

type KPW = KPW.Precise
object KPW extends CurrencyClass(CurrencyCode.KPW)

type KRW = KRW.Precise
object KRW extends CurrencyClass(CurrencyCode.KRW)

type KWD = KWD.Precise
object KWD extends CurrencyClass(CurrencyCode.KWD)

type KYD = KYD.Precise
object KYD extends CurrencyClass(CurrencyCode.KYD)

type KZT = KZT.Precise
object KZT extends CurrencyClass(CurrencyCode.KZT)

type LAK = LAK.Precise
object LAK extends CurrencyClass(CurrencyCode.LAK)

type LBP = LBP.Precise
object LBP extends CurrencyClass(CurrencyCode.LBP)

type LKR = LKR.Precise
object LKR extends CurrencyClass(CurrencyCode.LKR)

type LRD = LRD.Precise
object LRD extends CurrencyClass(CurrencyCode.LRD)

type LSL = LSL.Precise
object LSL extends CurrencyClass(CurrencyCode.LSL)

type LYD = LYD.Precise
object LYD extends CurrencyClass(CurrencyCode.LYD)

type MAD = MAD.Precise
object MAD extends CurrencyClass(CurrencyCode.MAD)

type MDL = MDL.Precise
object MDL extends CurrencyClass(CurrencyCode.MDL)

type MKD = MKD.Precise
object MKD extends CurrencyClass(CurrencyCode.MKD)

type MMK = MMK.Precise
object MMK extends CurrencyClass(CurrencyCode.MMK)

type MNT = MNT.Precise
object MNT extends CurrencyClass(CurrencyCode.MNT)

type MOP = MOP.Precise
object MOP extends CurrencyClass(CurrencyCode.MOP)

type MUR = MUR.Precise
object MUR extends CurrencyClass(CurrencyCode.MUR)

type MVR = MVR.Precise
object MVR extends CurrencyClass(CurrencyCode.MVR)

type MWK = MWK.Precise
object MWK extends CurrencyClass(CurrencyCode.MWK)

type MXN = MXN.Precise
object MXN extends CurrencyClass(CurrencyCode.MXN)

type MYR = MYR.Precise
object MYR extends CurrencyClass(CurrencyCode.MYR)

type MZN = MZN.Precise
object MZN extends CurrencyClass(CurrencyCode.MZN)

type NAD = NAD.Precise
object NAD extends CurrencyClass(CurrencyCode.NAD)

type NGN = NGN.Precise
object NGN extends CurrencyClass(CurrencyCode.NGN)

type NIO = NIO.Precise
object NIO extends CurrencyClass(CurrencyCode.NIO)

type NOK = NOK.Precise
object NOK extends CurrencyClass(CurrencyCode.NOK)

type NPR = NPR.Precise
object NPR extends CurrencyClass(CurrencyCode.NPR)

type NZD = NZD.Precise
object NZD extends CurrencyClass(CurrencyCode.NZD)

type OMR = OMR.Precise
object OMR extends CurrencyClass(CurrencyCode.OMR)

type PAB = PAB.Precise
object PAB extends CurrencyClass(CurrencyCode.PAB)

type PEN = PEN.Precise
object PEN extends CurrencyClass(CurrencyCode.PEN)

type PGK = PGK.Precise
object PGK extends CurrencyClass(CurrencyCode.PGK)

type PHP = PHP.Precise
object PHP extends CurrencyClass(CurrencyCode.PHP)

type PKR = PKR.Precise
object PKR extends CurrencyClass(CurrencyCode.PKR)

type PLN = PLN.Precise
object PLN extends CurrencyClass(CurrencyCode.PLN)

type PYG = PYG.Precise
object PYG extends CurrencyClass(CurrencyCode.PYG)

type QAR = QAR.Precise
object QAR extends CurrencyClass(CurrencyCode.QAR)

type RON = RON.Precise
object RON extends CurrencyClass(CurrencyCode.RON)

type RSD = RSD.Precise
object RSD extends CurrencyClass(CurrencyCode.RSD)

type RUB = RUB.Precise
object RUB extends CurrencyClass(CurrencyCode.RUB)

type RWF = RWF.Precise
object RWF extends CurrencyClass(CurrencyCode.RWF)

type SAR = SAR.Precise
object SAR extends CurrencyClass(CurrencyCode.SAR)

type SBD = SBD.Precise
object SBD extends CurrencyClass(CurrencyCode.SBD)

type SCR = SCR.Precise
object SCR extends CurrencyClass(CurrencyCode.SCR)

type SDG = SDG.Precise
object SDG extends CurrencyClass(CurrencyCode.SDG)

type SEK = SEK.Precise
object SEK extends CurrencyClass(CurrencyCode.SEK)

type SGD = SGD.Precise
object SGD extends CurrencyClass(CurrencyCode.SGD)

type SHP = SHP.Precise
object SHP extends CurrencyClass(CurrencyCode.SHP)

type SLE = SLE.Precise
object SLE extends CurrencyClass(CurrencyCode.SLE)

type SLL = SLL.Precise
object SLL extends CurrencyClass(CurrencyCode.SLL)

type SOS = SOS.Precise
object SOS extends CurrencyClass(CurrencyCode.SOS)

type SRD = SRD.Precise
object SRD extends CurrencyClass(CurrencyCode.SRD)

type SSP = SSP.Precise
object SSP extends CurrencyClass(CurrencyCode.SSP)

type STN = STN.Precise
object STN extends CurrencyClass(CurrencyCode.STN)

type SVC = SVC.Precise
object SVC extends CurrencyClass(CurrencyCode.SVC)

type SYP = SYP.Precise
object SYP extends CurrencyClass(CurrencyCode.SYP)

type SZL = SZL.Precise
object SZL extends CurrencyClass(CurrencyCode.SZL)

type THB = THB.Precise
object THB extends CurrencyClass(CurrencyCode.THB)

type TJS = TJS.Precise
object TJS extends CurrencyClass(CurrencyCode.TJS)

type TMT = TMT.Precise
object TMT extends CurrencyClass(CurrencyCode.TMT)

type TND = TND.Precise
object TND extends CurrencyClass(CurrencyCode.TND)

type TOP = TOP.Precise
object TOP extends CurrencyClass(CurrencyCode.TOP)

type TRY = TRY.Precise
object TRY extends CurrencyClass(CurrencyCode.TRY)

type TTD = TTD.Precise
object TTD extends CurrencyClass(CurrencyCode.TTD)

type TWD = TWD.Precise
object TWD extends CurrencyClass(CurrencyCode.TWD)

type TZS = TZS.Precise
object TZS extends CurrencyClass(CurrencyCode.TZS)

type UAH = UAH.Precise
object UAH extends CurrencyClass(CurrencyCode.UAH)

type UGX = UGX.Precise
object UGX extends CurrencyClass(CurrencyCode.UGX)

type USD = USD.Precise
object USD extends CurrencyClass(CurrencyCode.USD)

type UYU = UYU.Precise
object UYU extends CurrencyClass(CurrencyCode.UYU)

type UZS = UZS.Precise
object UZS extends CurrencyClass(CurrencyCode.UZS)

type VES = VES.Precise
object VES extends CurrencyClass(CurrencyCode.VES)

type VND = VND.Precise
object VND extends CurrencyClass(CurrencyCode.VND)

type VUV = VUV.Precise
object VUV extends CurrencyClass(CurrencyCode.VUV)

type WST = WST.Precise
object WST extends CurrencyClass(CurrencyCode.WST)

type XAF = XAF.Precise
object XAF extends CurrencyClass(CurrencyCode.XAF)

type XCD = XCD.Precise
object XCD extends CurrencyClass(CurrencyCode.XCD)

type XOF = XOF.Precise
object XOF extends CurrencyClass(CurrencyCode.XOF)

type XPF = XPF.Precise
object XPF extends CurrencyClass(CurrencyCode.XPF)

type YER = YER.Precise
object YER extends CurrencyClass(CurrencyCode.YER)

type ZAR = ZAR.Precise
object ZAR extends CurrencyClass(CurrencyCode.ZAR)

type ZMW = ZMW.Precise
object ZMW extends CurrencyClass(CurrencyCode.ZMW)

type ZWL = ZWL.Precise
object ZWL extends CurrencyClass(CurrencyCode.ZWL)
