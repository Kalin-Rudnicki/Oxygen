package oxygen.core

given idToSpecified: [A] => Conversion[A, Specified.WasSpecified[A]] = Specified.WasSpecified(_)
given aToBSpecified: [A, B] => (conv: Conversion[A, B]) => Conversion[A, Specified.WasSpecified[B]] = a => Specified.WasSpecified(conv(a))

given convertStringToColorString: Conversion[String, ColorString] = str => ColorString(ColorState.empty, str :: Nil)
