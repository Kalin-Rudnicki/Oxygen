package oxygen.ui.web.create

import scalajs.js

object value extends TypedObjectAttrBuilder[String]("value")(identity(_))

//////////////////////////////////////////////////////////////////////////////////////////////////////
//      Builder(s)
//////////////////////////////////////////////////////////////////////////////////////////////////////

abstract class TypedObjectAttrBuilder[T](key: String)(toAny: T => js.Any) { self =>
  final def :=(value: T): ObjectAttr = Widget.raw.objectAttr(key, toAny(value))
}

abstract class ObjectAttrBuilder(key: String) { self =>
  final def :=(value: js.Any): ObjectAttr = Widget.raw.objectAttr(key, value)
}
