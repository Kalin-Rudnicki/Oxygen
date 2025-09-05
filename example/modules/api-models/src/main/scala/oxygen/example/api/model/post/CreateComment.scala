package oxygen.example.api.model.post

import oxygen.schema.JsonSchema

final case class CreateComment(
    comment: String,
) derives JsonSchema
