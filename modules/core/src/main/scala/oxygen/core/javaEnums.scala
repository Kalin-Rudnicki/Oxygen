package oxygen.core

import java.time.Month
import java.time.format.TextStyle
import java.util.Locale
import oxygen.core.collection.NonEmptyList

object javaEnums {

  given monthEnumCompanion: Enum.Companion[Month] =
    Enum.Companion
      .fromArray(Month.values())
      .withDefaultToString { month =>
        NonEmptyList.of(TextStyle.FULL, TextStyle.SHORT).map { month.getDisplayName(_, Locale.US) }.appended(month.getValue.toString).distinct
      }

}
