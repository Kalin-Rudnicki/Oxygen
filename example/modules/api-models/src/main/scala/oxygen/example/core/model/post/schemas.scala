package oxygen.example.core.model.post

import oxygen.schema.*

given postIdSchema: PlainTextSchema[PostId] = PlainTextSchema.uuid.transform(PostId(_), _.id)
given commentIdSchema: PlainTextSchema[CommentId] = PlainTextSchema.uuid.transform(CommentId(_), _.id)
