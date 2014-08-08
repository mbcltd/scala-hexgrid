package hex

case class HexGrid[T](m:Map[Cube,T] = Map.empty) extends Map[Cube,T] {

  override def +[B1 >: T](kv: (Cube, B1)): HexGrid[B1] = HexGrid(m + kv)

  override def get(key: Cube): Option[T] = m.get(key)

  override def iterator: Iterator[(Cube, T)] = m.iterator

  override def -(key: Cube): Map[Cube, T] = m - key
}
