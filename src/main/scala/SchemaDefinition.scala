import sangria.marshalling.sprayJson._
import spray.json.DefaultJsonProtocol._

import sangria.execution.deferred.{Fetcher, HasId}
import sangria.schema._
import sangria.macros.derive._
import scala.concurrent.Future

import sangria.schema._



/**
 * Defines a GraphQL schema for the current project
 */
object SchemaDefinition {
  /**
    * Resolves the lists of characters. These resolutions are batched and
    * cached for the duration of a query.
    */
  val characters = Fetcher.caching(
    (ctx: CharacterRepo, ids: Seq[String]) ⇒
      Future.successful(ids.flatMap(id ⇒ ctx.getHuman(id) orElse ctx.getDroid(id))))(HasId(_.id))

  val EpisodeEnum = EnumType(
    "Episode",
    Some("One of the films in the Star Wars Trilogy"),
    List(
      EnumValue("NEWHOPE",
        value = Episode.NEWHOPE,
        description = Some("Released in 1977.")),
      EnumValue("EMPIRE",
        value = Episode.EMPIRE,
        description = Some("Released in 1980.")),
      EnumValue("JEDI",
        value = Episode.JEDI,
        description = Some("Released in 1983."))))



val LengthUnitEnum = EnumType(
    "LengthUnit",
    Some("Units of height"),
    List(
      EnumValue("METER",
        value = LengthUnit.METER,
        description = Some("The standard unit around the world")),
      EnumValue("FOOT",
        value = LengthUnit.FOOT,
        description = Some("Primarily used in the United States"))
        )
)

  val Character: InterfaceType[CharacterRepo, Character] =
    InterfaceType(
      "Character",
      "A character in the Star Wars Trilogy",
      () ⇒ fields[CharacterRepo, Character](
        Field("id", StringType,
          Some("The id of the character."),
          resolve = _.value.id),
        Field("name", OptionType(StringType),
          Some("The name of the character."),
          resolve = _.value.name),
        Field("friends", ListType(Character),
          Some("The friends of the character, or an empty list if they have none."),
          resolve = ctx ⇒ characters.deferSeqOpt(ctx.value.friends)),
        Field("appearsIn", OptionType(ListType(OptionType(EpisodeEnum))),
          Some("Which movies they appear in."),
          resolve = _.value.appearsIn map (e ⇒ Some(e)))
      ))

  val Human =
    ObjectType(
      "Human",
      "A humanoid creature in the Star Wars universe.",
      interfaces[CharacterRepo, HumanClass](Character),
      fields[CharacterRepo, HumanClass](
        Field("id", StringType,
          Some("The id of the human."),
          resolve = _.value.id),
        Field("name", OptionType(StringType),
          Some("The name of the human."),
          resolve = _.value.name),
        Field("friends", ListType(Character),
          Some("The friends of the human, or an empty list if they have none."),
          resolve = ctx ⇒ characters.deferSeqOpt(ctx.value.friends)),
        Field("appearsIn", OptionType(ListType(OptionType(EpisodeEnum))),
          Some("Which movies they appear in."),
          resolve = _.value.appearsIn map (e ⇒ Some(e))),
        Field("homePlanet", OptionType(StringType),
          Some("The home planet of the human, or null if unknown."),
          resolve = _.value.homePlanet)
      ))

  val Droid = ObjectType(
    "Droid",
    "A mechanical creature in the Star Wars universe.",
    interfaces[CharacterRepo, DroidClass](Character),
    fields[CharacterRepo, DroidClass](
      Field("id", StringType,
        Some("The id of the droid."),
        tags = ProjectionName("_id") :: Nil,
        resolve = _.value.id),
      Field("name", OptionType(StringType),
        Some("The name of the droid."),
        resolve = ctx ⇒ Future.successful(ctx.value.name)),
      Field("friends", ListType(Character),
        Some("The friends of the droid, or an empty list if they have none."),
        resolve = ctx ⇒ characters.deferSeqOpt(ctx.value.friends)),
      Field("appearsIn", OptionType(ListType(OptionType(EpisodeEnum))),
        Some("Which movies they appear in."),
        resolve = _.value.appearsIn map (e ⇒ Some(e))),
      Field("primaryFunction", OptionType(StringType),
        Some("The primary function of the droid."),
        resolve = _.value.primaryFunction)
    ))

  val Review = ObjectType ("Review",
    "Represents a review for a movie.",
    fields[CharacterRepo, ReviewClass](
      Field("stars", IntType,
        Some("The number of stars this review gave, 1-5"),
        resolve = _.value.stars),
      Field("commentary", OptionType(StringType),
        Some("Comment about the movie"),
        resolve = _.value.commentary),
    ))
  val LengthUnitArg = Argument("unit",    OptionInputType(LengthUnitEnum),
    description = "If omitted, returns length in Meters.")
  val Starship = ObjectType(
    "Starship",
    "",
    fields[CharacterRepo, StarshipClass](
      Field("id", StringType,
        Some("The id of the starship."),
        tags = ProjectionName("_id") :: Nil,
        resolve = _.value.id),
      Field("name", OptionType(StringType),
        Some("The name of the starship."),
        resolve = ctx ⇒ Future.successful(ctx.value.name)),
      Field("length", FloatType,
        arguments = LengthUnitArg :: Nil,
        resolve = (ctx) ⇒ ctx.ctx.getLength(ctx.value.length, ctx.arg(LengthUnitArg)))

    ))
  val SearchResultType =  UnionType[CharacterRepo]("SearchResult",types = Human :: Droid :: Starship ::Nil)

//  val SearchType = UnionType[Unit] ("SearchResult", types = List[Human, Droid])
  val ID = Argument("id", StringType, description = "id of the character")
  val textArg = Argument("text", StringType)

  val EpisodeArg = Argument("episode", OptionInputType(EpisodeEnum),
    description = "If omitted, returns the hero of the whole saga. If provided, returns the hero of that particular episode.")
  val EpisodeArgMand = Argument("episode", EpisodeEnum,
    description = "If omitted, returns the hero of the whole saga. If provided, returns the hero of that particular episode.")





  val Query = ObjectType(
    "Query", fields[CharacterRepo, Unit](
      Field("hero", Character,
        arguments = EpisodeArg :: Nil,
        resolve = (ctx) ⇒ ctx.ctx.getHero(ctx.arg(EpisodeArg))),
      Field("reviews", ListType(Review),
        arguments = EpisodeArgMand :: Nil,
        resolve = (ctx) ⇒ ctx.ctx.getReviews(ctx arg(EpisodeArgMand))),
      Field("human", OptionType(Human),
        arguments = ID :: Nil,
        resolve = ctx ⇒ ctx.ctx.getHuman(ctx arg ID)),
      Field("droid", Droid,
        arguments = ID :: Nil,
        resolve = Projector((ctx, f) ⇒ ctx.ctx.getDroid(ctx arg ID).get)),
      Field("character", OptionType(Character),
        arguments = ID :: Nil,
        resolve = ctx ⇒ ctx.ctx.getCharacter(ctx arg ID)),
      Field("search", ListType(OptionType(SearchResultType)),
        arguments = textArg :: Nil,
        resolve = (ctx) ⇒ ctx.ctx.search(ctx.arg(textArg))),
      Field("starship", OptionType(Starship),
        arguments = ID :: Nil,
        resolve = ctx ⇒ ctx.ctx.getStarship(ctx arg ID))
    ))



  implicit val reviewFormat = jsonFormat2(ReviewClass)
  implicit val ReviewInput: InputObjectType[ReviewClass] = deriveInputObjectType[ReviewClass](
    InputObjectTypeName("ReviewInput")
  )

  implicit val EpisodeEnumInput = EnumType(
    "Episode",
    Some("One of the films in the Star Wars Trilogy"),
    List(
      EnumValue("NEWHOPE",
        value = Episode.NEWHOPE,
        description = Some("Released in 1977.")),
      EnumValue("EMPIRE",
        value = Episode.EMPIRE,
        description = Some("Released in 1980.")),
      EnumValue("JEDI",
        value = Episode.JEDI,
        description = Some("Released in 1983."))))


   val EpisodeArgInputMand= Argument("episode", EpisodeEnumInput,
    description = "If omitted, returns the hero of the whole saga. If provided, returns the hero of that particular episode.")

   var ReviewInputArgMan = Argument("ReviewInput",ReviewInput)

  val Mutation = ObjectType(
    "Mutation",
    fields[CharacterRepo, Unit](
      Field("createReview",
        OptionType(Review),
        arguments = EpisodeArgInputMand :: ReviewInputArgMan :: Nil,
        resolve = (ctx) => ctx.ctx.createReview(ctx.arg(EpisodeArgInputMand), ctx.arg(ReviewInputArgMan))
      )
    )
  )

  val StarWarsSchema = Schema(Query,Some(Mutation))
}
