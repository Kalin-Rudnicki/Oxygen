package oxygen.ui.web.component

import oxygen.ui.web.Widget
import scalajs.js

object value extends TypedObjectAttrBuilder[String]("value")(identity(_))

//////////////////////////////////////////////////////////////////////////////////////////////////////
//      Builder(s)
//////////////////////////////////////////////////////////////////////////////////////////////////////

abstract class TypedObjectAttrBuilder[T](key: String)(toAny: T => js.Any) { self =>
  final def :=(value: T): Widget.Raw.ObjectAttr = Widget.Raw.objectAttr(key, toAny(value))
}

abstract class ObjectAttrBuilder(key: String) { self =>
  final def :=(value: js.Any): Widget.Raw.ObjectAttr = Widget.Raw.objectAttr(key, value)
}
