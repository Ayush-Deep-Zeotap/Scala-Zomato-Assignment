import org.scalatest.FunSuite
import zomato.Restaurant

class zomatoSuite extends FunSuite {

   //Dummy data for testing
  val dummyData: List[Restaurant] = List(
    Restaurant("0", "A", 3.5, 300, "Mumbai", "Cafe", List("Pastry","Coffee"), List("Italian"), "200"),
    Restaurant("1", "B", 4.5, 100, "Delhi", "Cafe",  List("Pastry","Ice-Cream","Coffee"), List("Mixed"), "300"),
    Restaurant("2", "C", 4.2, 200, "Delhi", "Cafe", List("Pastry","Coffee"), List("North-Indian","Mexican"), "150"),
    Restaurant("3", "D", 1.0, 350, "Delhi", "Pizzeria", List("Pastry","Coffee"), List("North-Indian","Italian"), "100"),
    Restaurant("4", "E", 4.4, 230, "Mumbai", "Bar", List("Pastry","Coffee"), List("Chinese"), "200"),
    Restaurant("5", "F", 4.2, 120, "Delhi", "Pizzeria", List("Pastry","Coffee","Biscuits"), List("Chinese","Italian"), "100")
  )

  test("Test : Top N restaurants by rating") {

    assert(zomato.sortByRating(2, dummyData) === List(
      Restaurant("1", "B", 4.5, 100, "Delhi", "Cafe",  List("Pastry","Ice-Cream","Coffee"), List("Mixed"), "300"),
      Restaurant("4", "E", 4.4, 230, "Mumbai", "Bar", List("Pastry","Coffee"), List("Chinese"), "200"))
    )

  }

  test("Test : Top N restaurants by rating in a given location and restaurant type") {

    assert(zomato.locationAndTypeFilter(3, dummyData, "Cafe", "Delhi") === List(
      Restaurant("1", "B", 4.5, 100, "Delhi", "Cafe",  List("Pastry","Ice-Cream","Coffee"), List("Mixed"), "300"),
      Restaurant("2", "C", 4.2, 200, "Delhi", "Cafe", List("Pastry","Coffee"), List("North-Indian","Mexican"), "150")
     )

    )

  }

  test("Test : Top N restaurants by rating and least number of votes in a given location") {

    assert(zomato.filterByVote(4, dummyData, "Delhi") === List(
      Restaurant("1", "B", 4.5, 100, "Delhi", "Cafe",  List("Pastry","Ice-Cream","Coffee"), List("Mixed"), "300"),
      Restaurant("5", "F", 4.2, 120, "Delhi", "Pizzeria", List("Pastry","Coffee","Biscuits"), List("Chinese","Italian"), "100"),
      Restaurant("2", "C", 4.2, 200, "Delhi", "Cafe", List("Pastry","Coffee"), List("North-Indian","Mexican"), "150"),
      Restaurant("3", "D", 1.0, 350, "Delhi", "Pizzeria", List("Pastry","Coffee"), List("North-Indian","Italian"), "100")
    ))

  }

  test("Test : No. of dishes liked in every restaurant") {

    assert(zomato.likedDishCount(dummyData) === List(2, 3, 2, 2, 2, 3))

  }
  test("Test : No. of distinct locations") {

    assert(zomato.distinctLocationCount(dummyData) === 2)

  }

  test("Test : No. of distinct cuisines at a certain location") {

    assert(zomato.distinctCuisine(dummyData, "Delhi") === 4)

  }

  test("Test : No. of distinct cuisines at each location") {

    assert(zomato.allDistinctCuisine(dummyData) === Map("Delhi" -> 4, "Mumbai" -> 2)
    )

  }

  test("Test : Count of restaurants for each cuisine type") {

    assert(zomato.countByCuisine(dummyData) === List(("Italian",3), ("Mixed",1), ("North-Indian",2), ("Mexican",1), ("Chinese",2))
    )
  }


}
