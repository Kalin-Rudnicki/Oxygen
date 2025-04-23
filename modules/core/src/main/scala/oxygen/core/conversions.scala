package oxygen.core

given idToSpecified: [A] => Conversion[A, Specified.WasSpecified[A]] = Specified.WasSpecified(_)

given convertStringToColorString: Conversion[String, ColorString] = str => ColorString(ColorState.empty, str :: Nil)
