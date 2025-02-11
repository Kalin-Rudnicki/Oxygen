package oxygen.test

abstract class OxygenSpecDefault extends OxygenSpec[Any] {
  override def layerProvider: LayerProvider[R] = LayerProvider.Empty
}
