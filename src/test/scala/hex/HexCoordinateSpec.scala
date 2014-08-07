package hex

import org.scalatest._
import org.scalatest.prop.TableDrivenPropertyChecks._

class HexCoordinateSpec extends FlatSpec with Matchers  {

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

  it should "have six neighbours" in {
    Cube.origin.neighbours.length shouldEqual 6
  }

  it should "have the correct neighbours" in {
    Cube.origin.north shouldEqual Cube(0,1,-1)
    Cube.origin.northWest shouldEqual Cube(-1,1,0)
    Cube.origin.southWest shouldEqual Cube(-1,0,1)
    Cube.origin.south shouldEqual Cube(0,-1,1)
    Cube.origin.southEast shouldEqual Cube(1,-1,0)
    Cube.origin.northEast shouldEqual Cube(1,0,-1)
  }

  "Axial coordinates" should "have the correct neighbours" in {
    Axial(0,0).north shouldEqual Axial(0,-1)
    Axial(0,0).northWest shouldEqual Axial(-1,0)
    Axial(0,0).southWest shouldEqual Axial(-1,1)
    Axial(0,0).south shouldEqual Axial(0,1)
    Axial(0,0).southEast shouldEqual Axial(1,0)
    Axial(0,0).northEast shouldEqual Axial(1,-1)
  }

  it should "Allow the addition of other Axial coordinates" in {
    Axial(5,1) + Axial(3,10) shouldEqual Axial(8,11)
  }

}
