package oxygen.storage.inMemory

// TODO (KR) : Create a parent StorageError in `oxygen-storage`, `cause: Option[Throwable]` where QueryError can be shoved
final case class ConstraintViolation(tableName: String, constraintName: String, keyValue: Any) extends Throwable {
  override def getMessage: String = s"Constraint violation on table `$tableName` ($constraintName) : $keyValue"
  override def toString: String = getMessage
}
