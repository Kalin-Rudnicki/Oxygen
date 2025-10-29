package oxygen.storage.inMemory

// TODO (KR) : Create a parent StorageError in `oxygen-storage`, `cause: Option[Throwable]` where QueryError can be shoved
final case class NoRecordWithKey(tableName: String, keyValue: Any) extends Throwable {
  override def getMessage: String = s"No record with given key in table `$tableName` : $keyValue"
  override def toString: String = getMessage
}
