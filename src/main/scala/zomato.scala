import scala.io.Source

object zomato extends App {

  case class Restaurant(id: String, restaurantName: String, rating: Double = 0.0, numberOfVotes: Long = 0L, location: String, restaurantType: String,
                        dishesLiked: List[String], typesOfCuisine: List[String], costForTwo: String)

  // Function to Read the Data
  def readData(): List[Restaurant] = {

    val src = Source.fromFile("data/zomato.csv")
    val lines = src.getLines().map(x => x.split(',')).toList
    src.close()
    val validResturants = lines.filter(x => x.length==9)
    validResturants.map(x => Restaurant(x(0),x(1),
      try{
        x(2).split('/').head.toDouble}
      catch{
        case e : NumberFormatException => 0.0
    },x(3) match {
        case votes if(votes.isEmpty) => 0L
        case _ => x(3).toLong
      },x(4),x(5),x(6) match {
        case dishesLiked if(dishesLiked.isEmpty) => List()
        case _ => x(6).split(',').toList
      },x(7) match {
        case cuisineList if(cuisineList.isEmpty) => List()
        case _ => x(7).split(' ').toList
      },x(8) match {
        case costForTwo if(costForTwo.isEmpty) => "Nil"
        case _ =>  x(8)
      }))

  }

  // Top N restaurants by rating
  def sortByRating(N: Int, restaurantList: List[Restaurant]): List[Restaurant] = restaurantList.sortBy(x => x.rating).reverse.take(N)

  // Top N restaurants by rating in a given location and restaurant type
  def locationAndTypeFilter(N: Int, restaurantList: List[Restaurant], reqType: String, reqLocation: String): List[Restaurant] =
    restaurantList.filter(x => x.location.equals(reqLocation) && x.restaurantType.equals(reqType)).sortBy(x => x.rating).reverse.take(N)

  // Top N restaurants by rating and least number of votes in a given location
  def filterByVote(N: Int, restaurantList: List[Restaurant], reqLocation: String): List[Restaurant] = {

    val restaurantsAtLocation = restaurantList.filter(x => x.location.contains(reqLocation))

    restaurantsAtLocation.sortBy(x => x.numberOfVotes.toInt).sortWith(_.rating > _.rating)

  }

  // No. of dishes liked in every restaurant
  def likedDishCount(restaurantList : List[Restaurant]): List[Int] = {

    restaurantList.map(x => x.dishesLiked.distinct.length)

  }

  //No. of distinct locations
  def distinctLocationCount(restaurantList : List[Restaurant]): Int = {

    restaurantList.map(x => x.location).distinct.length

  }

  // No. of distinct cuisines at a certain location
  def distinctCuisine(restaurantList : List[Restaurant],reqLocation: String): Int = {

    restaurantList.filter(x => x.location==reqLocation).map(x => x.typesOfCuisine).distinct.length

  }

  //No. of distinct cuisines at each location
  def allDistinctCuisine(restaurantList : List[Restaurant]): Map[String,Int] = {

    restaurantList.groupBy(_.location).map({case (k,v) => k -> v.map(x => x.typesOfCuisine).distinct.length})

  }

  //Count of restaurants for each cuisine type
  def countByCuisine(restaurantList : List[Restaurant]): List[(String,Int)] = {

    val cuisines = restaurantList.flatMap(_.typesOfCuisine).distinct

    cuisines.map({case x => x -> restaurantList.count(resturant => resturant.typesOfCuisine.contains(x))})

  }
  val zomatoData = readData()

  val res = countByCuisine(zomatoData)

  println(res)

}
