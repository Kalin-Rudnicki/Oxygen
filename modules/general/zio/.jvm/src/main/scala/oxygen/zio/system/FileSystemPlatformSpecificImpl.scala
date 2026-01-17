package oxygen.zio.system

trait FileSystemPlatformSpecificImpl { self: FileSystemPlatformSpecific =>

  override val default: FileSystem = JavaFileSystem

}
