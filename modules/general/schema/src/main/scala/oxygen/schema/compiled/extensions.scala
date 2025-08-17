package oxygen.schema.compiled

import oxygen.predef.core.*

extension (self: TypeTag[? <: AnyKind])
  private[compiled] def toSchemaType: SchemaType =
    SchemaType.fromTypeRef(self.tag)
