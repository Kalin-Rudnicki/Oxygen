package oxygen.http.model

import oxygen.schema.*
import zio.http.*

given urlSchema: PlainTextSchema[URL] =
  PlainTextSchema.string.transformOption(URL.decode(_).toOption, _.encode)
