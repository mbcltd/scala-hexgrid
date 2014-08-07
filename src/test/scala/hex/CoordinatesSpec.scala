package hex

import org.scalatest._
import org.scalatest.prop.TableDrivenPropertyChecks._

class CoordinatesSpec extends FlatSpec with Matchers  {

  val testCubes = Table(
    ("x","z"),
    (0,0),
    (1,1),
    (-1,1),
    (5,20),
    (20,5),
    (0,-10),
    (-5,-5)
  )

  "Cube coordinates" should "map to and from Axial coordinates" in {
    forAll( testCubes ) {
      (x: Int, z: Int) => Cube(x, z) shouldEqual Cube(x, z).toAxial.toCube
    }
  }

  it should "map to and from Even Q coordinates" in {
    forAll( testCubes ) {
      (x: Int, z: Int) => Cube(x, z) shouldEqual Cube(x, z).toEvenQ.toCube
    }
  }

  it should "map to and from Odd Q coordinates" in {
    forAll( testCubes ) {
      (x: Int, z: Int) => Cube(x, z) shouldEqual Cube(x, z).toOddQ.toCube
    }
  }

  it should "map to and from Even R coordinates" in {
    forAll( testCubes ) {
      (x: Int, z: Int) => Cube(x, z) shouldEqual Cube(x, z).toEvenR.toCube
    }
  }

  it should "map to and from Odd R coordinates" in {
    forAll( testCubes ) {
      (x: Int, z: Int) => Cube(x, z) shouldEqual Cube(x, z).toOddR.toCube
    }
  }

  it should "allow addition of other cubes" in {
    Cube(0,0,0) + Cube(1,0,-1) shouldEqual Cube(1,0,-1)
    Cube(1,0,-1) + Cube(0,0,0) shouldEqual Cube(1,0,-1)
    Cube(1,1,-2) + Cube(1,-1,0) shouldEqual Cube(2,0,-2)
  }

}
