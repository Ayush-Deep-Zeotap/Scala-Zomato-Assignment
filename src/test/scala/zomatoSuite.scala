import org.scalatest.FunSuite
import zomato.Restaurant

import scala.collection.mutable._


class zomatoSuite extends FunSuite {

  // Dummy data for testing
  val dummyData: ListBuffer[Restaurant] = ListBuffer(
    Restaurant("0", "A", "3.5/5.0", "300", "Mumbai", "Cafe", "Pastry Coffee", "Italian", "200"),
    Restaurant("1", "B", "4.5/5.0", "100", "Delhi", "Cafe", "Pastry Ice-Cream Coffee", "Mixed", "300"),
    Restaurant("2", "C", "4.2/5.0", "200", "Delhi", "Cafe", "Pastry Coffee", "North-Indian Mexican", "150"),
    Restaurant("3", "D", "1.0/5.0", "350", "Delhi", "Pizzeria", "Pastry Coffee", "North-Indian Italian", "100"),
    Restaurant("4", "E", "4.4/5.0", "230", "Mumbai", "Bar", "Pastry Coffee", "Chinese", "200"),
    Restaurant("5", "F", "4.2/5.0", "120", "Delhi", "Pizzeria", "Pastry Coffee Biscuits", "Chinese Italian", "100")
  )

  test("Test : Top N restaurants by rating") {

    assert(zomato.sortByRating(2, dummyData) === ListBuffer(
      Restaurant("1", "B", "4.5/5.0", "100", "Delhi", "Cafe", "Pastry Ice-Cream Coffee", "Mixed", "300"),
      Restaurant("4", "E", "4.4/5.0", "230", "Mumbai", "Bar", "Pastry Coffee", "Chinese", "200"))
    )

  }

  test("Test : Top N restaurants by rating in a given location and restaurant type") {

    assert(zomato.locationAndTypeFilter(3, dummyData, "Cafe", "Delhi") === ListBuffer(
      Restaurant("1", "B", "4.5/5.0", "100", "Delhi", "Cafe", "Pastry Ice-Cream Coffee", "Mixed", "300"),
      Restaurant("2", "C", "4.2/5.0", "200", "Delhi", "Cafe", "Pastry Coffee", "North-Indian Mexican", "150")
     )

    )

  }

  test("Test : Top N restaurants by rating and least number of votes in a given location") {

    assert(zomato.filterByVote(4, dummyData, "Delhi") === ListBuffer(
      Restaurant("1", "B", "4.5/5.0", "100", "Delhi", "Cafe", "Pastry Ice-Cream Coffee", "Mixed", "300"),
      Restaurant("5", "F", "4.2/5.0", "120", "Delhi", "Pizzeria", "Pastry Coffee Biscuits", "Chinese Italian", "100"),
      Restaurant("2", "C", "4.2/5.0", "200", "Delhi", "Cafe", "Pastry Coffee", "North-Indian Mexican", "150"),
      Restaurant("3", "D", "1.0/5.0", "350", "Delhi", "Pizzeria", "Pastry Coffee", "North-Indian Italian", "100"),
    ))

  }

  test("Test : No. of dishes liked in every restaurant") {

    assert(zomato.likedDishCount(dummyData) === ListBuffer(2, 3, 2, 2, 2, 3))

  }
  test("Test : No. of distinct locations") {

    assert(zomato.distinctLocationCount(dummyData) === 2)

  }

  test("Test : No. of distinct cuisines at a certain location") {

    assert(zomato.distinctCuisine(dummyData, "Delhi") === 4)

  }

  test("Test : No. of distinct cuisines at each location") {

    assert(zomato.allDistinctCuisine(dummyData) === List(4,2)
    )

  }

  test("Test : Count of restaurants for each cuisine type") {

    assert(zomato.countByCuisine(dummyData) === ListBuffer(("Italian",3), ("Mixed",1), ("North-Indian",2), ("Mexican",1), ("Chinese",2))
    )
  }


}
