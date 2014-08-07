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
    (0,-10)
  )

  "Cube coordinates" should "map to and from Axial coordinates" in {
    forAll( testCubes ) {
      (x: Int, z: Int) => Cube(x, z) shouldEqual Cube(x, z).toAxial.toCube
    }
  }

  "Cube coordinates" should "map to and from EvenQ coordinates" in {
    forAll( testCubes ) {
      (x: Int, z: Int) => Cube(x, z) shouldEqual Cube(x, z).toEvenQ.toCube
    }
  }

}
