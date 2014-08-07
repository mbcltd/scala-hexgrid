package hex

object Cube {
  def apply(x:Int, z:Int):Cube = this( x, 0-x-z, z )
}

case class Cube(x:Int, y:Int, z:Int) {
  def toAxial = Axial( x, z )
  def toEvenQ = EvenQ( x, z + (x + (x&1)) / 2 )
  def toOddQ  = OddQ ( x, z + (x - (x&1)) / 2 )
  def toEvenR = EvenR( x + (z + (z&1)) / 2, z )
  def toOddR  = OddR ( x + (z - (z&1)) / 2, z )

  def +(c:Cube):Cube = Cube(x+c.x, y+c.y, z+c.z)
}

case class Axial(q:Int, r:Int) {
  def toCube = Cube( q, r )
}

case class EvenQ(q:Int, r:Int) {
  def toCube = Cube( q, r - (q + (q&1)) / 2 )
}

case class OddQ(q:Int, r:Int) {
  def toCube = Cube( q, r - (q - (q&1)) / 2 )
}

case class EvenR(q:Int, r:Int) {
  def toCube = Cube( q - (r + (r&1)) / 2, r )
}

case class OddR(q:Int, r:Int) {
  def toCube = Cube( q - (r - (r&1)) / 2, r )
}


