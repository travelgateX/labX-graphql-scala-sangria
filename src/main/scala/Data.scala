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

case class Human(
  id: String,
  name: Option[String],
  friends: List[String],
  appearsIn: List[Episode.Value],
  homePlanet: Option[String],
  height: Double,
  mass: Int,
  starShip: List[String]) extends Character

case class Droid(
  id: String,
  name: Option[String],
  friends: List[String],
  appearsIn: List[Episode.Value],
  primaryFunction: Option[String]) extends Character

case class StarShip (
  id: String,
  name: String,
  length: Double
)

case class Review(
                  stars: Int,
                  commentary: Option[String],
                  episode: Episode.Value
                 )

class CharacterRepo {
  import CharacterRepo._

  def getHero(episode: Option[Episode.Value]) =
    episode flatMap (_ ⇒ getHuman("1000")) getOrElse droids.last

  def getHuman(id: String): Option[Human] = humans.find(c ⇒ c.id == id)

  def getReviews(episode: Episode.Value): List[Review] = reviews.filter(c ⇒ c.episode == episode)

  def getDroid(id: String): Option[Droid] = droids.find(c ⇒ c.id == id)
}

object CharacterRepo {
  var reviews = List[Review](
/*    Review(
      stars = 0,
      commentary = Some("1"),
      episode = Episode.EMPIRE
    ),
      Review(
      stars = 0,
      commentary = Some("2"),
      episode = Episode.EMPIRE
    ),
      Review(
      stars = 0,
      commentary = Some("3"),
      episode = Episode.JEDI
    )*/
  )

  val humans = List(
    Human(
      id = "1000",
      name = Some("Luke Skywalker"),
      friends = List("1002", "1003", "2000", "2001"),
      appearsIn = List(Episode.NEWHOPE, Episode.EMPIRE, Episode.JEDI),
      homePlanet = Some("Tatooine"),
      height = 1.72,
      mass = 77,
      starShip = List("3001","3003")),
    Human(
      id = "1001",
      name = Some("Darth Vader"),
      friends = List("1004"),
      appearsIn = List(Episode.NEWHOPE, Episode.EMPIRE, Episode.JEDI),
      homePlanet = Some("Tatooine"),
      height = 2.02,
      mass = 136,
      starShip = List("3002")),
    Human(
      id = "1002",
      name = Some("Han Solo"),
      friends = List("1000", "1003", "2001"),
      appearsIn = List(Episode.NEWHOPE, Episode.EMPIRE, Episode.JEDI),
      homePlanet = None,
      height = 1.8,
      mass = 80,
      starShip = List("3000","3003")),
    Human(
      id = "1003",
      name = Some("Leia Organa"),
      friends = List("1000", "1002", "2000", "2001"),
      appearsIn = List(Episode.NEWHOPE, Episode.EMPIRE, Episode.JEDI),
      homePlanet = Some("Alderaan"),
      height = 1.5,
      mass = 49,
      starShip = List("")),
    Human(
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
    Droid(
      id = "2000",
      name = Some("C-3PO"),
      friends = List("1000", "1002", "1003", "2001"),
      appearsIn = List(Episode.NEWHOPE, Episode.EMPIRE, Episode.JEDI),
      primaryFunction = Some("Protocol")),
    Droid(
      id = "2001",
      name = Some("R2-D2"),
      friends = List("1000", "1002", "1003"),
      appearsIn = List(Episode.NEWHOPE, Episode.EMPIRE, Episode.JEDI),
      primaryFunction = Some("Astromech"))
  )

  val startShips = List(
    StarShip(
        id = "3000",
        name = "Millennium Falcon",
        length = 34.37
    ),
    StarShip(
      id = "3001",
      name = "X-Wing",
      length = 12.5
    ),
    StarShip(
      id = "3002",
      name = "TIE Advanced x1",
      length = 9.2
    ),
    StarShip(
      id = "3003",
      name = "Imperial shuttle",
      length = 20
    )
  )
}
