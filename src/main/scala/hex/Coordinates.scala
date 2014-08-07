package hex

object Cube {
  def apply(x:Int, z:Int):Cube = this(x, 0-x-z, z)
}

case class Cube(x:Int, y:Int, z:Int) {
  def toAxial = Axial(x, z)
  def toEvenQ = EvenQ(x, z + (x + (x&1)) / 2 )
}

case class Axial(q:Int, r:Int) {
  def toCube = Cube(q, r)
}

case class EvenQ(q:Int, r:Int) {
  def toCube = Cube(q, r - (q + (q&1)) / 2)
}

