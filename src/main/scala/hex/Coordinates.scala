package hex


trait HexCoordinate[T] {
  def toCube:Cube
  def fromCube(c:Cube):T

  def north:T =     this + Cube(0,1,-1)
  def northWest:T = this + Cube(-1,1,0)
  def southWest:T = this + Cube(-1,0,1)
  def south:T =     this + Cube(0,-1,1)
  def southEast:T = this + Cube(1,-1,0)
  def northEast:T = this + Cube(1,0,-1)

  def neighbours:Seq[T] = north :: northWest :: southWest :: south :: southEast :: northEast :: Nil

  def +(c:HexCoordinate[_]):T = fromCube( toCube.add(c.toCube) )
}

object Cube {
  def apply(x:Int, z:Int):Cube = this( x, 0-x-z, z )
}

case class Cube(x:Int, y:Int, z:Int) extends HexCoordinate[Cube] {
  def toAxial = Axial( x, z )
  def toEvenQ = EvenQ( x, z + (x + (x&1)) / 2 )
  def toOddQ  = OddQ ( x, z + (x - (x&1)) / 2 )
  def toEvenR = EvenR( x + (z + (z&1)) / 2, z )
  def toOddR  = OddR ( x + (z - (z&1)) / 2, z )

  def toCube = this
  def fromCube(c:Cube) = c

  def add(c:Cube) = Cube(x+c.x, y+c.y, z+c.z)
}

case class Axial(q:Int, r:Int) {
  def toCube = Cube( q, r )
  def fromCube(c:Cube) = c.toAxial
}

case class EvenQ(q:Int, r:Int) {
  def toCube = Cube( q, r - (q + (q&1)) / 2 )
  def fromCube(c:Cube) = c.toEvenQ
}

case class OddQ(q:Int, r:Int) {
  def toCube = Cube( q, r - (q - (q&1)) / 2 )
  def fromCube(c:Cube) = c.toOddQ
}

case class EvenR(q:Int, r:Int) {
  def toCube = Cube( q - (r + (r&1)) / 2, r )
  def fromCube(c:Cube) = c.toEvenR
}

case class OddR(q:Int, r:Int) {
  def toCube = Cube( q - (r - (r&1)) / 2, r )
  def fromCube(c:Cube) = c.toOddR
}


