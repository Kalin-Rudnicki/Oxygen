package oxygen.zio.system

trait CommandServicePlatformSpecificImpl { self: CommandServicePlatformSpecific =>

  override val default: CommandService = JavaCommandService

}
