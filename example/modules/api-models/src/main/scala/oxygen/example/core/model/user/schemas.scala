package oxygen.example.core.model.user

import oxygen.schema.*

given emailSchema: PlainTextSchema[Email] = PlainTextSchema.string.transformOrFail(Email.parseEither, _.email)
given userIdSchema: PlainTextSchema[UserId] = PlainTextSchema.uuid.transform(UserId(_), _.id)
