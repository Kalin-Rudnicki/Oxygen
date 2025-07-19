package oxygen.http.model

final case class ContentType(`type`: String, subType: String) {
  val show: String = s"${`type`}/$subType"
  val showBytes: Array[Byte] = show.getBytes
  override def toString: String = show
}
object ContentType {
  val PlainText: ContentType = ContentType("text", "plain")
  val Html: ContentType = ContentType("text", "html")
  val Css: ContentType = ContentType("text", "css")
  val Csv: ContentType = ContentType("text", "csv")
  val Javascript: ContentType = ContentType("text", "javascript")
  val Json: ContentType = ContentType("application", "json")
  val Xml: ContentType = ContentType("application", "xml")
  val Xhtml: ContentType = ContentType("application", "xhtml+xml")
  val Atom: ContentType = ContentType("application", "atom+xml")
  val Rss: ContentType = ContentType("application", "rss+xml")
  val Pdf: ContentType = ContentType("application", "pdf")
  val Zip: ContentType = ContentType("application", "zip")
  val Gzip: ContentType = ContentType("application", "gzip")
  val FormUrlEncoded: ContentType = ContentType("application", "x-www-form-urlencoded")
  val OctetStream: ContentType = ContentType("application", "octet-stream")
  val JavascriptApp: ContentType = ContentType("application", "javascript")
  val JsonLd: ContentType = ContentType("application", "ld+json")
  val MsWord: ContentType = ContentType("application", "msword")
  val MsWordOpenXml: ContentType = ContentType("application", "vnd.openxmlformats-officedocument.wordprocessingml.document")
  val MsExcel: ContentType = ContentType("application", "vnd.ms-excel")
  val MsExcelOpenXml: ContentType = ContentType("application", "vnd.openxmlformats-officedocument.spreadsheetml.sheet")
  val MsPowerpoint: ContentType = ContentType("application", "vnd.ms-powerpoint")
  val MsPowerpointOpenXml: ContentType = ContentType("application", "vnd.openxmlformats-officedocument.presentationml.presentation")
  val Png: ContentType = ContentType("image", "png")
  val Jpeg: ContentType = ContentType("image", "jpeg")
  val Gif: ContentType = ContentType("image", "gif")
  val Webp: ContentType = ContentType("image", "webp")
  val Svg: ContentType = ContentType("image", "svg+xml")
  val Bmp: ContentType = ContentType("image", "bmp")
  val Tiff: ContentType = ContentType("image", "tiff")
  val MpegAudio: ContentType = ContentType("audio", "mpeg")
  val OggAudio: ContentType = ContentType("audio", "ogg")
  val WavAudio: ContentType = ContentType("audio", "wav")
  val WebmAudio: ContentType = ContentType("audio", "webm")
  val Mp4Video: ContentType = ContentType("video", "mp4")
  val MpegVideo: ContentType = ContentType("video", "mpeg")
  val WebmVideo: ContentType = ContentType("video", "webm")
  val OggVideo: ContentType = ContentType("video", "ogg")
  val MultipartFormData: ContentType = ContentType("multipart", "form-data")

}
