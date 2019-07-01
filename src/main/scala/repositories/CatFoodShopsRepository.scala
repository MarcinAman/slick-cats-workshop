package repositories

import model.domain.PriceList.Entry
import model.domain.{CatFoodPrice, CatFoodShop, PriceList}
import model.infra.{CatFoodPrices, CatFoodShops, CatFoods}
import slick.jdbc.H2Profile.api._

import scala.concurrent.ExecutionContext

class CatFoodShopsRepository(
  catFoodsRepository: CatFoodsRepository
) extends IdRepository[CatFoodShop, CatFoodShops](CatFoodShops.query) {

  def findByName(name: String)(implicit ec: ExecutionContext): DBIO[Option[CatFoodShop]] = {
    query.filter(_.name === name).result.headOption
  }

  /**
    * Write a method that will compose PriceList for a shop with given Id
    *
    * Hint: you can try for {} yield {} syntax on DBIO
    * Hint#2: try DBIO.sequence
    * @param shopId id of shop for which prices should be found
    */
  def getPriceList(shopId: Long)(implicit ec: ExecutionContext): DBIO[PriceList] = {
    for {
      s <- CatFoodShops.query.filter(_.id === shopId).result.head
      e <- getEntries(shopId)
    } yield PriceList(s, e)
  }

  private def getEntries(shopId: Long)(implicit ec: ExecutionContext): DBIO[Seq[PriceList.Entry]] = {
    val q = for {
      shop <- CatFoodShops.query.filter(_.id === shopId)
      price <- CatFoodPrices.query.filter(_.shopId === shop.id)
      food <- CatFoods.query.filter(_.id === price.foodId)
    } yield (food, price.price)

    q.result.map {
      a =>
        a.map {
          case (food, price) => PriceList.Entry(food, price)
        }
    }
  }

  private def toEntry(catFoodPrice: CatFoodPrice)(implicit ec: ExecutionContext): DBIO[PriceList.Entry] = {
    CatFoods.query
      .filter(_.id === catFoodPrice.foodId)
      .result
      .map(f => Entry(f.head, catFoodPrice.pricePerGram))
  }

}
