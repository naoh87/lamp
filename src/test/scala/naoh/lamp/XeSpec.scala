package naoh.lamp

import org.scalatest.MustMatchers
import org.scalatest.WordSpec

class XeSpec extends WordSpec with MustMatchers {
  "find" should {
    "attr" in {
      val book = (Xe \ "catalog" \ "book").attr("id")(_ =="bk101")
      (book.find(XmlSample.book) \ "title").map(_.text) mustBe Seq("XML Developer's Guide")
    }
    "at" in {
      val book = (Xe \ "catalog" \ "book").attr("id")(_ =="bk102") at (Xe \ "title")
      book.find(XmlSample.book).map(_.text) mustBe Seq("Midnight Rain")
    }
    "empty" in {
      val book = (Xe \ "catalog" \ "book").attr("id")(_ =="bk102") at (Xe \ "nonsense")
      book.find(XmlSample.book).map(_.text) mustBe Seq()
    }

    "condition" in {
      val book = (Xe \ "catalog" \ "book") on (Xe \ "book" \ "price").text(_.toFloat < 5) at (Xe \ "title")
      book.find(XmlSample.book).map(_.text) mustBe Seq("Lover Birds", "Splish Splash", "Creepy Crawlies")
    }
  }

  "set" should {
    "conditional" in {
      val book = (Xe \ "catalog" \ "book") on (Xe \ "book" \ "price").text(_.toFloat < 5) at (Xe \ "title")
      val result = book.set(<title2>removed</title2>)(XmlSample.book)
      book.find(result).map(_.text) mustBe Seq.empty
    }

  }

  "transform" should {
    "conditional" in {
      val book = (Xe \ "catalog" \ "book") on (Xe \ "book" \ "price").text(_.toFloat < 5) at (Xe \ "title")
      val result = book.transform(n => <title>[Sale] {n.text}</title>)(XmlSample.book)
      book.find(result).map(_.text) mustBe Seq("[Sale] Lover Birds", "[Sale] Splish Splash", "[Sale] Creepy Crawlies")
    }
  }

}
