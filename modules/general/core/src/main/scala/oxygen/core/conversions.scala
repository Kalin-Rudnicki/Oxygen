package oxygen.core

given convertStringToColorString: Conversion[String, ColorString] = str => ColorString(ColorState.empty, str :: Nil)
