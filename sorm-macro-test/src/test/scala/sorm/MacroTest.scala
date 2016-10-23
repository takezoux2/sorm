package sorm

import org.scalatest.{FlatSpec, Matchers}
import sorm._

/**
  * Created by takezoux2 on 2016/10/24.
  */
class MacroTest extends FlatSpec with Matchers {

  object Db extends Instance(
    entities = Set(Entity[Hoge]()),
    url = "jdbc:h2:mem:test"
  )
  for(id <- 1 to 10) {
    Db.save(Hoge("Hoge:" + id))
  }

  def assertList(rs: List[Hoge with Persisted])(ids : Int*) = {
    assert(rs.size == ids.size)
    val sorted = rs.sortBy(_.id)
    for(i <- 0 until ids.size) {
      assert(sorted(i).id == ids(i),s"Wrong id for ${sorted(i)}")
      assert(sorted(i).name == s"Hoge:${ids(i)}",s"Wrong name for ${sorted(i)}")
    }
  }


  "whereEqual" should "be called by macro" in {
    {
      val q = Db.query[Hoge].whereEqual(_.id, 1L)
      assertList(q.fetch().toList)(1)
    }

    {
      val q = Db.query[Hoge].whereEqual(_.name, "Hoge:1")
      assertList(q.fetch().toList)(1)
    }
  }

  "whereNotEqual" should "be called by macro" in {
    val q = Db.query[Hoge].whereNotEqual(_.id, 1L)
    assertList(q.fetch().toList)((2 to 10):_*)
  }

  "whereLarger" should "be called by macro" in {
    val q = Db.query[Hoge].whereLarger(_.id, 5L)
    println(q.fetch().toList)
    assertList(q.fetch().toList)((6 to 10):_*)
  }
  "whereLargerOrEqual" should "be called by macro" in {
    val q = Db.query[Hoge].whereLargerOrEqual(_.id, 5L)
    println(q.fetch().toList)
    assertList(q.fetch().toList)((5 to 10):_*)
  }
  "whereSmaller" should "be called by macro" in {
    val q = Db.query[Hoge].whereSmaller(_.id, 5L)
    println(q.fetch().toList)
    assertList(q.fetch().toList)((1 to 4):_*)
  }
  "whereSmallerOrEqual" should "be called by macro" in {
    val q = Db.query[Hoge].whereSmallerOrEqual(_.id, 5L)
    println(q.fetch().toList)
    assertList(q.fetch().toList)((1 to 5):_*)
  }
  "whereLike" should "be called by macro" in {
    val q = Db.query[Hoge].whereLike(_.name, "%2")
    println(q.fetch().toList)
    assertList(q.fetch().toList)(2)
  }
  "whereNotLike" should "be called by macro" in {
    val q = Db.query[Hoge].whereNotLike(_.name, "%2")
    println(q.fetch().toList)
    assertList(q.fetch().toList)((1 :: (3 to 10).toList):_*)
  }
  "whereIn" should "be called by macro" in {
    val q = Db.query[Hoge].whereIn(_.id, Seq(1,2,3))
    println(q.fetch().toList)
    assertList(q.fetch().toList)((1 to 3):_*)
  }
  "whereNotIn" should "be called by macro" in {
    val q = Db.query[Hoge].whereNotIn(_.id, Seq(1,2,3,4,5,6,7))
    println(q.fetch().toList)
    assertList(q.fetch().toList)((8 to 10):_*)
  }

  "whereRegex" should "be called by macro" in {
    val q = Db.query[Hoge].whereRegex(_.name, ".+2")
    println(q.fetch().toList)
    assertList(q.fetch().toList)(2)
  }

  "whereNotRegex" should "be called by macro" in {
    val q = Db.query[Hoge].whereNotRegex(_.name, ".+2")
    println(q.fetch().toList)
    assertList(q.fetch().toList)((1 :: (3 to 10).toList):_*)
  }

}

case class Hoge(name: String)

