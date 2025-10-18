package oxygen.core

import java.time.Month
import java.time.format.TextStyle
import java.util.Locale
import oxygen.core.collection.NonEmptyList
import oxygen.core.typeclass.*

object javaEnums {

  given month: StrictEnum[Month] =
    StrictEnum.make[Month](
      Month.values().toSeq,
      month => NonEmptyList.of(TextStyle.FULL, TextStyle.SHORT).map { month.getDisplayName(_, Locale.US) }.appended(month.getValue.toString).distinct,
    )

}
