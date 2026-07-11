package oxygen.http.model

import oxygen.http.core.partial.ResponseCodecNoStatus
import oxygen.json.{jsonDiscriminator, Json, JsonEncoder}
import oxygen.predef.core.*
import oxygen.schema.JsonSchema
import zio.http.{Headers, MediaType}

final case class PageHtmlResponse(
    html: String,
    headers: Headers,
) {

  def addHeaders(headers: (String, String)*): PageHtmlResponse =
    this.copy(headers = this.headers.addHeaders(headers))

  def addSpecifiedHeaders(headers: (String, Specified[String])*): PageHtmlResponse =
    this.addHeaders(headers.flatMap { (key, value) => value.toOption.map((key, _)) }*)

  def addOptionalHeaders(headers: (String, Option[String])*): PageHtmlResponse =
    this.addHeaders(headers.flatMap { (key, value) => value.map((key, _)) }*)

  def addCacheHeaders(
      cacheControl: Specified[String] = ___,
      pragma: Specified[String] = ___,
      expires: Specified[String] = ___,
  ): PageHtmlResponse =
    this.addSpecifiedHeaders(
      "Cache-Control" -> cacheControl,
      "Pragma" -> pragma,
      "Expires" -> expires,
    )

}
object PageHtmlResponse {

  // TODO (KR) : Im thinking this should maybe be moved to the UI side of things
  //           : And potentially pages can have dynamic `favicon`, in a similar manner as `title`
  @jsonDiscriminator("type")
  sealed trait FaviconConfig derives JsonSchema {
    final def nonEmptyOption: Option[FaviconConfig.NonEmpty] = this match
      case self: FaviconConfig.NonEmpty => self.some
      case FaviconConfig.Empty          => None
  }
  object FaviconConfig {

    case object Empty extends FaviconConfig

    sealed trait NonEmpty extends FaviconConfig

    final case class HREF(href: String, imageType: Option[String], sizes: Option[String]) extends FaviconConfig.NonEmpty {
      val contentType: Option[MediaType] = calculateContentType(href, imageType)
    }

    private def calculateContentType(href: String, imageType: Option[String]): Option[MediaType] =
      imageType.flatMap(MediaType.forContentType).orElse {
        href.split('.').lastOption.flatMap(MediaType.forFileExtension)
      }

  }

  // TODO (KR) :
  // val oxygenDefaultUIConfig_varName: String = "oxygenDefaultUIConfig"

  val oxygenCustomUIConfig_varName: String = "oxygenCustomUIConfig"

  given responseCodec: ResponseCodecNoStatus[PageHtmlResponse] =
    (ResponseCodecNoStatus.body.constContentType(MediaType.text.html) ++ ResponseCodecNoStatus.header.raw).autoTransform[PageHtmlResponse]

  private def globalVarsScriptText(pairs: (String, Option[Json])*): Text = {
    val flattenedPairs: Seq[(String, Json)] =
      pairs.flatMap { (key, value) => value.map((key, _)) }

    Text.when(flattenedPairs.nonEmpty) {
      str"""
           |    <script>
           |        // Global Vars${Text.foreach(flattenedPairs) { (key, value) => str"\n        const $key = ${value.showCompact};" }}
           |        // Freeze Global Vars${Text.foreach(flattenedPairs) { (key, _) => str"\n        Object.freeze($key);" }}
           |    </script>"""
    }
  }

  private def faviconText(favicon: FaviconConfig): Text = {
    def keyValues(node: String, pairs: (String, Option[String])*): Text = {
      val flatPairs: Seq[(String, String)] = pairs.flatMap { (key, value) => value.map((key, _)) }
      str"\n    <$node${Text.foreach(flatPairs) { (key, value) => str" $key=\"$value\"" }}>"
    }

    Text.foreach(favicon.nonEmptyOption) {
      case favicon: FaviconConfig.HREF => keyValues("link", "rel" -> "icon".some, "href" -> favicon.href.some, "type" -> favicon.contentType.map(_.fullType), "sizes" -> favicon.sizes)
    }
  }

  def make(
      title: String,
      scriptPath: String,
      favicon: FaviconConfig,
  )(
      customJsonConfig: Option[Json],
  ): PageHtmlResponse = {
    val interpolateTags: Text =
      faviconText(favicon) ++
        globalVarsScriptText(oxygenCustomUIConfig_varName -> customJsonConfig)

    val html: Text =
      str"""<!DOCTYPE html>
           |<html lang="en">
           |
           |<head>
           |    <meta charset="UTF-8">
           |    <title>$title</title>$interpolateTags
           |    <script id="scripts" src="$scriptPath"></script>
           |</head>
           |
           |<body>
           |</body>
           |
           |</html>
           |"""

    PageHtmlResponse(html.toString, Headers.empty)
  }

  def makeWithoutConfig(
      title: String,
      scriptPath: String,
      favicon: FaviconConfig,
  ): PageHtmlResponse =
    make(title, scriptPath, favicon)(None)

  def makeWithConfig[A: JsonEncoder as enc](
      title: String,
      scriptPath: String,
      favicon: FaviconConfig,
  )(
      customConfig: A,
  ): PageHtmlResponse =
    make(title, scriptPath, favicon)(enc.encodeJsonAST(customConfig).some)

}
