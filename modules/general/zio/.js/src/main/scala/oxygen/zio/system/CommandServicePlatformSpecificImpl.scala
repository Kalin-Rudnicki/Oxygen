package oxygen.zio.system

trait CommandServicePlatformSpecificImpl { self: CommandServicePlatformSpecific =>

  // TODO (KR) : implement for real
  override val default: CommandService = UnimplementedCommandService

}
