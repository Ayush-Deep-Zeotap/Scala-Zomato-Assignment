import scala.collection.mutable._

object zomato extends App {

  case class Restaurant(id: String = "Nil", restaurantName: String = "", rating: String = "0.0/5.0", numberOfVotes: String = "0", location: String = "Unknown", restaurantType: String = "Nil",
                        dishesLiked: String = "", typesOfCuisines: String = "", costForTwo: String = "Nil")

  val zomatoData = ListBuffer[Restaurant]()

  // Function to Read the Data
  def readData() = {

    def splitRow(row: String): List[String] = {
      row.split(",(?=([^\"]*\"[^\"]*\")*[^\"]*$)", -1).toList
    }

    val bufferedSource = io.Source.fromFile("data/zomato.csv")
    for (line <- bufferedSource.getLines) {

      val row = splitRow(line)

      var validRating = ""

      if (row(2) == "NEW") validRating = "0.0/5.0"

      else validRating = row(2)

      val parsedRow = Restaurant(row.head, row(1), validRating, row(3), row(4), row(5), row(6), row(7), row(8))

      zomatoData += parsedRow
    }
    bufferedSource.close()
  }

  // Top N restaurants by rating
  def sortByRating(N: Int, restaurantList: ListBuffer[Restaurant]): ListBuffer[Restaurant] = restaurantList.sortBy(x => x.rating.split('/').head.toDouble).reverse.take(N)

  // Top N restaurants by rating in a given location and restaurant type
  def locationAndTypeFilter(N: Int, restaurantList: ListBuffer[Restaurant], reqType: String, reqLocation: String): ListBuffer[Restaurant] =
    restaurantList.filter(x => x.location.contains(reqLocation)).filter(x => x.restaurantType.contains(reqType)).take(N)

  // Top N restaurants by rating and least number of votes in a given location
  def filterByVote(N: Int, restaurantList: ListBuffer[Restaurant], reqLocation: String): ListBuffer[Restaurant] = {

    val restaurantsAtLocation = restaurantList.filter(x => x.location.contains(reqLocation))

    restaurantsAtLocation.sortBy(x => x.numberOfVotes.toInt).sortWith(_.rating.split('/').head.toDouble > _.rating.split('/').head.toDouble)

  }

  // No. of dishes liked in every restaurant
  def likedDishCount(restaurantList : ListBuffer[Restaurant]): ListBuffer[Int] = {

    restaurantList.map(x => x.dishesLiked.split( " ").length)

  }

  //No. of distinct locations
  def distinctLocationCount(restaurantList : ListBuffer[Restaurant]): Int = {

    val locations = restaurantList.map(x => x.location)

    locations.distinct.length

  }

  // No. of distinct cuisines at a certain location
  def distinctCuisine(restaurantList : ListBuffer[Restaurant],reqLocation: String): Int = {

    restaurantList.filter(x => x.location.contains(reqLocation)).groupBy(_.typesOfCuisines.split(" ")).size

  }

  //No. of distinct cuisines at each location
  def allDistinctCuisine(restaurantList : ListBuffer[Restaurant]): Any = {

    restaurantList.groupBy(_.location).map(x => x._2.length)

  }

  //Count of restaurants for each cuisine type
  def countByCuisine(restaurantList : ListBuffer[Restaurant]): Any = {

      val res = restaurantList.map(_.typesOfCuisines.split(" "));

      val cuisines = res.flatten.distinct

      cuisines.map(x => (x,restaurantList.count(_.typesOfCuisines.contains(x))) )

  }

}
