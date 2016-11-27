package naoh.lamp

import org.scalatest.FlatSpec
import org.scalatest.MustMatchers
import org.scalatest.WordSpec

class GlassSpec extends WordSpec with MustMatchers {
  "hoge" should {
    "gew" in {
      val xe = ((Xe \ "hoge" \ "few") on Lamp(s => (s \ "id").text == "123", "ID=123")) \ "crazy"
      val body =
        <hoge>
          <few><id>123</id><mol>momonga</mol></few>
          <few><id>123</id><crazy>123</crazy></few>
          <few><id>234</id><mol>momonga</mol></few>
          <aww><id>123</id><mol>momonga</mol></aww>
        </hoge>
      println(body)

      println("\n~~~~")
      val r = xe.set(<king>KING</king>)(body)

      println(r)
    }
  }

}
