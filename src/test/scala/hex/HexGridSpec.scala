package hex

import org.scalatest._
import HexCoordinateImplicits._

class HexGridSpec extends FlatSpec with Matchers {
  "A map of HexCoordinates" should "Support EvenQ coordinates" in {
    // Create a 10 by 10 set of EvenQ coordinates
    val c:Seq[EvenQ] = for(i <- 0 to 100) yield EvenQ(i%10,i/10)

    // Convert this into a map of cubes with Int values
    val m:Map[Cube,Int] = c.map( e => ( e.toCube, e.q+e.r ) ).toMap

    m.get( EvenQ(5,3) ) shouldEqual Some(8)
    m.get( EvenQ(5,3).north ) shouldEqual Some(7)


  }
}
