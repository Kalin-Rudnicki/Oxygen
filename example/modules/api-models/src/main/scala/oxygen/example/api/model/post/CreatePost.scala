package oxygen.example.api.model.post

import oxygen.schema.JsonSchema

final case class CreatePost(
    title: String,
    body: String,
) derives JsonSchema
