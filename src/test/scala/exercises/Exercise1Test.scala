package exercises

import model.domain.{Cat, Sex}
import model.infra.{Breeds, Cats}
import slick.dbio.DBIO
import slick.jdbc.H2Profile.api._
import slick.lifted.WrappingQuery
import util.WorkshopTest

import scala.concurrent.ExecutionContext.Implicits.global

class Exercise1Test extends WorkshopTest {

  /**
    * Exercise 1 - Play with slick queries
    *
    * In this exercise you will learn some basic ways of fetching data with use of slick queries.
    * Test cases below have method left for you to implement. Good luck!
    *
    * hint: https://bartosz822.github.io/slick-cats-workshop-presentation/#/8/7
    */

  /** find all cats that are older than 5*/
  def findOldCats: DBIO[Seq[Cat]] = {
    Cats.query.filter(_.age > 5).result
  }

  /** find all male cats that are older than 5*/
  def findOldMaleCats: DBIO[Seq[Cat]] = {
    Cats.query.filter(_.age > 5).filter(_.sex === Sex.Male).result
  }

  /** find all persian cats*/
  def findPersianCats: DBIO[Seq[Cat]] = {
    val query = for {
      b <- Breeds.query.filter(_.name === "Persian")
      c <- Cats.query.filter(_.breedId === b.id)
    } yield c

    query.result
  }

  /** find how many calories are needed to feed all old cats*/
  def findCaloricNeedsForOldCats: DBIO[BigDecimal] = {
    val query = for {
      c <- Cats.query if c.age > 5
      b <- Breeds.query.filter(_.id === c.breedId)
    } yield {
      b.caloriesPerDay
    }

    query.sum.result.map(_.getOrElse(BigDecimal(0)))
  }

  /** find how many calories are needed for cats grouped by their breed*/
  def findCaloricNedsForBreeds: DBIO[Seq[(String, BigDecimal)]] = {
    (for {
      c <- Cats.query
      b <- Breeds.query.filter(_.id === c.breedId)
    } yield (b.name, b.caloriesPerDay)).groupBy(_._1).map{ s =>
      (s._1, s._2.map(_._2).sum.getOrElse(BigDecimal(0)))
    }.result
  }

  "findOldCats" should "return cats with age greater than 5" in rollbackWithTestData {
    for {
      foundCats <- findOldCats
    } yield {
      foundCats.map(_.name) should contain theSameElementsAs Seq("Shadow", "Bailey", "Marley", "Boo")
    }
  }

  "findOldMaleCats" should "return male cats with age greater than 5" in rollbackWithTestData {
    for {
      foundCats <- findOldMaleCats
    } yield {
      foundCats.map(_.name) should contain theSameElementsAs Seq("Shadow", "Bailey", "Marley")
    }
  }

  "findPersianCats" should "return persian cats" in rollbackWithTestData {
    for {
      foundCats <- findPersianCats
    } yield {
      foundCats.map(_.name) should contain theSameElementsAs Seq("Gizmo", "Cali", "Noodle")
    }
  }

  "findCaloricNeedsForOldCats" should "sum of daily caloric needs for old cats" in rollbackWithTestData {
    for {
      caloriesSum <- findCaloricNeedsForOldCats
    } yield {
      caloriesSum shouldBe 2480
    }
  }

  "findCaloricNeedsForBreeds" should "sum of daily caloric needs for old cats" in rollbackWithTestData {
    for {
      caloriesSum <- findCaloricNedsForBreeds
    } yield {
      caloriesSum should contain theSameElementsAs Seq(
        "Abyssinian" -> 1000,
        "Devon Rex" -> 800,
        "Maine Coon" -> 2280,
        "Norwegian Forest Cat" -> 1220,
        "Ocicat" -> 1080,
        "Persian" -> 1860,
        "Russian Blue" ->1180,
        "Siamese" -> 1120,
        "Ukrainian Levkoy" -> 1300
      )
    }
  }

}
