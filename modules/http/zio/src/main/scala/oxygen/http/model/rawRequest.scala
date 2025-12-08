package oxygen.http.model

import oxygen.http.core.*
import zio.http.*

final case class RawRequestPath(
    method: Method,
    paths: List[String],
    headers: Headers,
    queryParams: QueryParams,
    body: ReadOnlyCachedHttpBody,
)
object RawRequestPath {

  given RequestCodec.PathLike[RawRequestPath] =
    (
      RequestCodec.anyMethod /
        RequestCodec.path.rest("paths") ++
        RequestCodec.header.rawFiltered ++
        RequestCodec.query.raw ++
        RequestCodec.body.raw
    ).autoTransform[RawRequestPath]

}

final case class RawRequestNonPath(
    method: Method,
    headers: Headers,
    queryParams: QueryParams,
    body: ReadOnlyCachedHttpBody,
)
object RawRequestNonPath {

  given RequestCodec.NonPathLike[RawRequestNonPath] =
    (
      RequestCodec.anyMethod ++
        RequestCodec.header.rawFiltered ++
        RequestCodec.query.raw ++
        RequestCodec.body.raw
    ).autoTransform[RawRequestNonPath]

}
