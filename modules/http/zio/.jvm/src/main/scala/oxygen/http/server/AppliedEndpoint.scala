package oxygen.http.server

import oxygen.http.schema.*
import zio.*
import zio.http.*
import zio.metrics.MetricLabel

final case class AppliedEndpoint(
    schema: EndpointSchema,
    handle: EndpointInput => Option[URIO[Scope, Option[Response]]],
) {

  val method: Option[Method] = schema.requestSchema.method
  def apiName: Option[String] = schema.apiName
  def endpointName: String = schema.endpointName
  def requestSchema: RequestSchema = schema.requestSchema
  def successResponseSchema: ResponseSchema = schema.successResponseSchema
  def errorResponseSchema: ResponseSchema = schema.errorResponseSchema
  def doc: Option[String] = schema.doc

  val fullName: String = apiName match
    case Some(apiName) => s"$apiName.$endpointName"
    case None          => endpointName

  // TODO (KR) : use these
  def metricLabels: Set[MetricLabel] = schema.metricLabels

}
