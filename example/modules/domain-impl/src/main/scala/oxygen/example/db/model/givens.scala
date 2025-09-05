package oxygen.example.db.model

import oxygen.example.core.model.post.*
import oxygen.example.core.model.user.*
import oxygen.sql.schema.*

given userIdRowRepr: RowRepr[UserId] = RowRepr.uuid.transform(UserId(_), _.id)
given postIdRowRepr: RowRepr[PostId] = RowRepr.uuid.transform(PostId(_), _.id)
given commentIdRowRepr: RowRepr[CommentId] = RowRepr.uuid.transform(CommentId(_), _.id)

given emailRowRepr: RowRepr[Email] = RowRepr.string.transform(Email.unsafeWrapWithoutValidation, _.email)
