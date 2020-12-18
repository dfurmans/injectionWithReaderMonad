trait Travel
case class SimpleTravel(desde: String, hasta: String) extends Travel

trait Repository[A] {
  def save(a: A): A
}

trait TravelRepository extends Repository[Travel]

object TravelRepositoryArangoDB extends TravelRepository {
  def save(tr: Travel): Travel = {
    println( "Here we could persist our data or send it further " + tr)
    tr
  }
}

trait TravelService {

  def createTravelRequest(from: String,
                          to: String
                         ): TravelRepository => Travel
}
// with simple function lifting
object TravelServiceImp extends TravelService {
  def createTravelRequest(from: String,
                          to: String
                         ): TravelRepository => Travel = {
    tr => {

      val aTravel = SimpleTravel(desde = from, hasta = to)
      tr.save(aTravel)
    }
  }
}

case class Reader[R, A](run: R => A) {
  def map[B](f: A => B): Reader[R, B] =
    Reader(r => f(run(r)))

  def flatMap[B](f: A => Reader[R, B]): Reader[R, B] =
    Reader(r => f(run(r)).run(r))
}

trait TravelService {

  def createTravelRequest(from: String,
                          to: String
                         ): Reader[TravelRepository, Travel]
}
// with a Monad Reader
object TravelServiceImpReader extends TravelService {
  def createTravelRequest(from: String,
                          to: String
                         ): Reader[TravelRepository, Travel] = {
    Reader(tr => {
      tr.save(SimpleTravel(desde = from, hasta = to))
    }
    )
  }
}

val misViajesDeSueno: Reader[TravelRepository, (Travel, Travel)] = for {
  a <- TravelServiceImpReader.createTravelRequest("OpenSouthCode", "Cádiz")
  b <- TravelServiceImpReader.createTravelRequest("Cádiz", "Granda")
} yield (a,b)

println(misViajesDeSueno.run)
val result: (Travel, Travel) = misViajesDeSueno.run(TravelRepositoryArangoDB)
println(result._1)
println(result._2)

