import SchemaDefinition.Review
import spray.json.DefaultJsonProtocol.jsonFormat2

import scala.collection.mutable._
object Episode extends Enumeration {
  val NEWHOPE, EMPIRE, JEDI = Value
}
object LengthUnit extends Enumeration {
  val METER, FOOT = Value
}
trait Character {
  def id: String
  def name: Option[String]
  def friends: List[String]
  def appearsIn: List[Episode.Value]
}

case class HumanClass(
  id: String,
  name: Option[String],
  friends: List[String],
  appearsIn: List[Episode.Value],
  homePlanet: Option[String],
  height: Double,
  mass: Int,
  starShip: List[String]) extends Character

case class DroidClass(
  id: String,
  name: Option[String],
  friends: List[String],
  appearsIn: List[Episode.Value],
  primaryFunction: Option[String]) extends Character

case class StarshipClass (
                      id: String,
                      name: String,
                      length: Double
                    )

case class ReviewClass(
                  stars: Int,
                  commentary: Option[String]
                 )


class CharacterRepo {

  import CharacterRepo._


  def getHuman(id: String): Option[HumanClass] = humans.find(c ⇒ c.id == id)

  def getStarship(id: String): Option[StarshipClass] = starships.find(c ⇒ c.id == id)

  def getReviews(episode: Episode.Value): List[ReviewClass] = reviews.getOrElse(episode, Nil)

  def getDroid(id: String): Option[DroidClass] = droids.find(c ⇒ c.id == id)

  def createReview(episode: Episode.Value, review: ReviewClass): Option[ReviewClass] = {
    reviews.get(episode) match {
      case Some(xs: List[ReviewClass]) => reviews.update(episode, xs :+ review)
      case None => reviews(episode) = List[ReviewClass] {
        review
      }
    }
    Option(review)
  }

  def getHero(episode: Option[Episode.Value]) =
    episode flatMap (_ ⇒ getHuman("1000")) getOrElse droids.last

  def getCharacter(id: String) : Option[Character] = {
    val h = getHuman(id)
    h match {
      case Some(_) => h
      case None => getDroid(id)
    }
  }

  def getLength(length: Double, unit: Option[LengthUnit.Value]) =
    unit match {
      case None => length
      case Some(x) => if (x == LengthUnit.METER) length else { length * 3 }
    }
  def search(text: String) : List[Option[CharacterRepo]] = {
    (humans.filter(h => (h.name match { case Some(x) => x.contains(text) case None => false }) || h.id.contains(text)) ::: droids.filter(d => (d.name match { case Some(x) => x.contains(text) case None => false }) || d.id.contains(text)) ::: starships.filter(s => s.name.contains(text) || s.id.contains(text))).asInstanceOf[List[Option[CharacterRepo]]]
  }

}


object CharacterRepo {
  var reviews = Map[Episode.Value, List[ReviewClass]](

  )

  val humans = List(
    HumanClass(
      id = "1000",
      name = Some("Luke Skywalker"),
      friends = List("1002", "1003", "2000", "2001"),
      appearsIn = List(Episode.NEWHOPE, Episode.EMPIRE, Episode.JEDI),
      homePlanet = Some("Tatooine"),
      height = 1.72,
      mass = 77,
      starShip = List("3001","3003")),
    HumanClass(
      id = "1001",
      name = Some("Darth Vader"),
      friends = List("1004"),
      appearsIn = List(Episode.NEWHOPE, Episode.EMPIRE, Episode.JEDI),
      homePlanet = Some("Tatooine"),
      height = 2.02,
      mass = 136,
      starShip = List("3002")),
    HumanClass(
      id = "1002",
      name = Some("Han Solo"),
      friends = List("1000", "1003", "2001"),
      appearsIn = List(Episode.NEWHOPE, Episode.EMPIRE, Episode.JEDI),
      homePlanet = None,
      height = 1.8,
      mass = 80,
      starShip = List("3000","3003")),
    HumanClass(
      id = "1003",
      name = Some("Leia Organa"),
      friends = List("1000", "1002", "2000", "2001"),
      appearsIn = List(Episode.NEWHOPE, Episode.EMPIRE, Episode.JEDI),
      homePlanet = Some("Alderaan"),
      height = 1.5,
      mass = 49,
      starShip = List("")),
    HumanClass(
      id = "1004",
      name = Some("Wilhuff Tarkin"),
      friends = List("1001"),
      appearsIn = List(Episode.NEWHOPE, Episode.EMPIRE, Episode.JEDI),
      homePlanet = None,
      height = 1.8,
      mass = 0,
      starShip = List(""))
  )

  val droids = List(
    DroidClass(
      id = "2000",
      name = Some("C-3PO"),
      friends = List("1000", "1002", "1003", "2001"),
      appearsIn = List(Episode.NEWHOPE, Episode.EMPIRE, Episode.JEDI),
      primaryFunction = Some("Protocol")),
    DroidClass(
      id = "2001",
      name = Some("R2-D2"),
      friends = List("1000", "1002", "1003"),
      appearsIn = List(Episode.NEWHOPE, Episode.EMPIRE, Episode.JEDI),
      primaryFunction = Some("Astromech"))
  )

  val starships = List(
    StarshipClass(
        id = "3000",
        name = "Millennium Falcon",
        length = 34.37
    ),
    StarshipClass(
      id = "3001",
      name = "X-Wing",
      length = 12.5
    ),
    StarshipClass(
      id = "3002",
      name = "TIE Advanced x1",
      length = 9.2
    ),
    StarshipClass(
      id = "3003",
      name = "Imperial shuttle",
      length = 20
    )
  )
}
