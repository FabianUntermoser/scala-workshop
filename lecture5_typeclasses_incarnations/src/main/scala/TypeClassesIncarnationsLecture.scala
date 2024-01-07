import PresentationUtil.{slide, _}
import japgolly.scalajs.react.ScalaComponent
import japgolly.scalajs.react.vdom.html_<^._
import org.scalajs.dom

import scala.scalajs.js.annotation.JSExport

object TypeClassesIncarnationsLecture {
  import Enumeration._

  val overview = chapter(
    chapterSlide(
      <.h1("Type Class Incarnations")
    ),

    slide(
      "What we will learn in this lecture",
      Enumeration(
        Item.stable("Categorical Programming"),
        Item.fadeIn("Typelevel Cats"),
        Item.fadeIn("Cats Kernel"),
        Item.fadeIn("Cats Core"),
        Item.fadeIn("Monad Transformers"),
        Item.fadeIn("Monad Instances")
      )
    ),

    noHeaderSlide(
      <.h2("You have a question?"),
      <.h3("Ask it right away!")
    )
  )

  val categorical = chapter(
    chapterSlide(
      <.h2("Categorical Programming")
    ),

    slide(
      "Categorical Programming",
      <.p("Using mathematical concepts from Category Theory to enhance our FP toolbox."),
      <.p(
        ^.cls := "fragment fade-in",
        "But what does that mean in practice?"
      )
    ),

    noHeaderSlide(
      <.h3("Let's have an example"),
      <.br,
      <.h4("Equality")
    ),

    slide(
      "Categorical FP: equality",
      <.p("Scala comes already with an equality method build into its objects. But that allows us to write the following:"),
      <.br,
      scalaC("""
        // equality between different types
        "hello" == 1

        // wrong inequality
        class Person(name: String)

        // yields `false`
        new Person("Gandalf") == new Person("Gandalf")
      """)
    ),

    slide(
      "Categorical FP: equality",
      <.p("We want an equality proof which is strict in its parameter types and tests value equality."),
      <.br,
      scalaC("""
        def eqv[A](a: A, b: A): Boolean
      """)
    ),

    slide(
      "Categorical FP: equality",
      <.p("We want that to be a polymorphic property."),
      <.br,
      scalaC("""
        trait Eq[A] {

          def eqv(a: A, b: A): Boolean
        }
      """)
    ),

    slide(
      "Categorical FP: equality",
      scalaC("""
        implicit val strEq = new Eq[String] {
          def eqv(a: String, b: String): Boolean = a == b
        }

        // doesn't compile
        implicitly[Eq[String]].eqv("hello", 1)
      """),
    ),

    slide(
      "Categorical FP: equality",
      scalaC("""
        implicit val personEq = new Eq[Person] {
          def eqv(a: Person, b: Person): Boolean = a.name == b.name
        }

        // yields `true`
        implicitly[Eq[Person]].eqv(
          new Person("Gandalf"), 
          new Person("Gandalf")
        )
      """)
    ),

    slide(
      "Categorical FP: equality",
      scalaC("""
        // add properties to type parameter
        def isFixedPoint[A: Eq](a: A)(f: A => A): Boolean = {
          implicitly[Eq[A]].eqv(a, f(a))
        }
      """)
    ),

    noHeaderSlide(
      <.h3("You know what?"),
      <.br,
      <.h5("There is a library with usefull type classes already available")
    )
  )

  val catsLibrary = chapter(
    chapterSlide(
      <.h2("Typelevel Cats")
    ),

    slide(
      "Cats",
      <.img(
        ^.alt   := "Cats Logo",
        ^.src   := "./img/cats-logo.png",
        ^.width := "40%"
      )
    ),

    slide(
      "Cats",
      <.p("Cats, short for ", <.strong("Cat"), "egorie", <.strong("s")),
      <.br,
      <.a(
        ^.href := "https://typelevel.org/cats",
        "https://typelevel.org/cats/"
      )
    ),

    slide(
      "Cats",
      Enumeration(
        Item.stable("a set of useful abstractions"),
        Item.fadeIn("syntax to use type classes more conveniently"),
        Item.fadeIn("additional tooling to scrap boilderplate and scrutinize instances")
      )
    ),

    noHeaderSlide(
      <.h3("Let's have a look into the project")
    )
  )

  val catsKernel = chapter(
    chapterSlide(
      <.h2("Cats Kernel")
    ),

    slide(
      "Cats Kernel",
      <.a(
        ^.href := "https://github.com/typelevel/cats/tree/master/kernel/src/main/scala/cats/kernel",
        "cats/kernel"
      )
    ),

    noHeaderSlide(
      <.h3("Equality")
    ),

    slide(
      "Cats Kernel: Eq",
      <.p("Equality is already build into the library."),
      <.br,
      scalaC("""
        trait Eq[A] {

          def eqv(x: A, y: A): Boolean

          def neqv(x: A, y: A): Boolean = !eqv(x, v)
        }
      """),
      scalaCFragment("""
        object Eq {

          // resolve instance for type `A`
          def apply[A](implicit ev: Eq[A]): Eq[A] = ev
        }
      """)
    ),

    slide(
      "Cats Kernel: Eq",
      scalaC("""
        import cats.kernel.Eq
        import cats.implicits._

        Eq[Int].eqv(1, 2) == Eq.apply[Int].eqv(1, 2)
                          == intEq.eqv(1, 2)
                          == false
      """),
      scalaCFragment("""
        // doesn't compile
        Eq[Int].eqv("hello", 2) 
      """)
    ),

    slide(
      "Cats Kernel: Eq",
      scalaC("""
        // Cats also comes with some convenient syntax
        1 === 2

        // equal to
        Eq[Int].eqv(1, 2)
      """)
    ),

    slide(
      "Cats Kernel: Eq",
      <.p("There are instances for all primitive types and many collections like List, Set, etc."),
      <.br,
      <.p(
        ^.cls := "fragment fade-in",
        "That is true for all Kernel type classes."
      )
    ),

    noHeaderSlide(
      <.h3("After equality follows order")
    ),

    slide(
      "Cats Kernel: Order",
      scalaC("""
        trait Order[A] extends Eq[A] {

          /** Result of comparing `x` with `y`. Returns an Int whose sign is:
            *  - negative iff `x < y`
            *  - zero     iff `x = y`
            *  - positive iff `x > y`
            */
          def compare(x: A, y: A): Int

          ...
        }
      """)
    ),

    slide(
      "Cats Kernel: Order",
      scalaC("""
        import cats.kernel.Order

        Order[Int].compare(1, 2) === -1


        import cats.kernel.Comparison.LessThan

        Order[Int].comparison(1, 2) === LessThan
      """)
    ),

    slide(
      "Cats Kernel: Order",
      scalaC("""
        def largestToStr[A: Order](x: A, b: A): String = 
          Order[A].comparison(x, y) match {
            case GreaterThan => s"x: $x"
            case EqualTo     => s"x and y are equal: $x"
            case LessThan    => s"y: $y"
          }


        largestToStr(2, 1) === "x: 2"
      """)
    ),

    noHeaderSlide(
      <.h3("We compared - now we combine"),
      <.br,
      <.h4("Semigroup")
    ),

    slide(
      "Cats Kernel: Semigroup",
      scalaC("""
        trait Semigroup[A] {

          def combine(x: A, y: A): A

          ...
        }
      """)
    ),

    slide(
      "Cats Kernel: Semigroup",
      scalaC("""
        import cats.kernel.Semigroup

        Semigroup[Int].combine(1, 2) === 3


        // again we have some convenient syntax
        1 |+| 2 === 3
      """)
    ),

    slide(
      "Cats Kernel: Semigroup",
      scalaC("""
        def largeEnough[A: Semigroup: Order](threshold: A)
                                            (x: A, y: A): Boolean =
          Order[A].gt(Semigroup[A].combine(x, y), threshold)


        val le = largeEnough(5)

        le(1, 2) === false
      """)
    ),

    noHeaderSlide(
      <.h3("But wait, there is more"),
      <.br,
      <.h4("Semigroup follows some laws")
    ),

    slide(
      "Cats Kernel: Semigroup laws",
      scalaC("""
        // associativity
        a |+| (b |+| c) == (a |+| b) |+| c
      """)
    ),

    slide(
      "Cats Kernel: laws",
      <.p("Kernel laws are checked with a special test suit in kernel-laws.")
    ),

    exerciseSlide(
      "Let's Code: Kernel",
      bash("""
        sbt> project typeclasses-incarnations-exercises
        sbt> test:testOnly exercise5.Kernel
      """)
    ),

    noHeaderSlide(
      <.h3("We had type classes for simple types"),
      <.br,
      <.h4("Let's continue with higher order types")
    )
  )

  val catsCore = chapter(
    chapterSlide(
      <.h2("Cats Core")
    ),

    slide(
      "Cats Core",
      <.a(
        ^.href := "https://github.com/typelevel/cats/tree/master/core/src/main/scala/cats",
        "cats/core"
      )
    ),

    slide(
      "Cats Core",
      <.p("Imagine the following situation:"),
      <.br,
      scalaC("""
        case class Person(name: String)

        def names(persons: List[Person]): List[String] = 
          persons.map(_.name)
      """)
    ),

    slide(
      "Cats Core",
      <.p("But what if the context is unknown aka you  make this function generic in `F[_]`?")
    ),

    slide(
      "Cats Core",
      <.p("`F[_]` is too generic. No operations are attached."),
      <.br,
      scalaC("""
        def names[F[_]](persons: F[Person]): F[String] = ???
      """)
    ),

    noHeaderSlide(
      <.h3("Functor")
    ),

    slide(
      "Cats Core: Functor",
      scalaC("""
        trait Functor[F[_]] {

          def map[A, B](fa: F[A])(f: A => B): F[B]
 
          ...
        }
      """)
    ),

    slide(
      "Cats Core: Functor",
      scalaC("""
        import cats.Functor
        import cats.implicits._

        Functor[List].map(List(1, 2))(_ + 1) === List(2, 3)
      """)
    ),

    slide(
      "Cats Core: Functor",
      scalaC("""
        // it comes again with some convenient syntax
        // `fmap` is from Cats
        List(1, 2).fmap(_ + 1) === List(2, 3)

        // `map` is build into `List`
        List(1, 2).map(_ + 1)  === List(2, 3)
      """)
    ),

    slide(
      "Cats Core: Functor",
      scalaC("""
        def names[F[_]: Functor](persons: F[Person]): F[String] = 
          Functor[F].map(persons)(_.name)


        names(List(Person("Gandalf"))) === List("Gandalf")
        names(Some(Person("Gandalf"))) === Some("Gandalf")
      """)
    ),

    noHeaderSlide(
      <.h3("We have laws against")
    ),

    slide(
      "Cats Core: Functor laws",
      scalaC("""
        // composition
        f.map(g).map(h) == f.map(g.compose(h))

        // identity
        f.map(identity) == f
      """)
    ),

    slide(
      "Cats Core: Functor composition",
      <.p("We can compose Functors as needed."),
      <.br,
      scalaC("""
        implicit val listOpt = Functor[List].compose[Option]

        List(Some(1))).fmap(_ + 1) === List(Some(2))
      """),
      scalaCFragment("""
        names(List(Some(Person("Gandalf")))) === List(Some("Gandalf"))
      """)
    ),

    slide(
      "Cats Core: Functor instances",
      <.p("There are instances for many Scala collections like Option, Either, List, etc.")
    ),

    noHeaderSlide(
      <.h3("But I have a mapping function with more than one parameter!"),
      <.br,
      <.h3(
        ^.cls := "fragment fade-in",
        "Applicative"
      )
    ),

    slide(
      "Cats Core: Applicative",
      scalaC("""
        val combine: Int => Int => Int = a => b => a + b

        // only allows functions with arity 1
        Some(1).fmap(combine) === Some(b => 1 + b)
      """)
    ),

    slide(
      "Cats Core: Applicative",
      scalaC("""
        trait Applicative[F] extends Functor[F] {

          def pure[A](a: A): F[A]

          def ap[A, B](ff: F[A => B])(fa: F[A]): F[B]

          ...
        }
      """)
    ),

    slide(
      "Cats Core: Applicative",
      scalaC("""
        import cats.Applicative

        // lift a value
        Applicative[Option].pure(1) === Some(1)

        // apply a function in context `F[_]`
        Applicative[Option].ap[Int, Int](Some(_ + 1))(Some(1)) === Some(2)
      """)
    ),

    slide(
      "Cats Core: Applicative",
      scalaC("""
        val combine: Int => Int => Int = a => b => a + b

        // only allows functions with arity 1
        Some(1).fmap(combine) === Some(b => 1 + b)

        // apply remaining parameters
        Some(1).fmap(combine).ap(Some(2)) === Some(3)
      """),
      scalaCFragment("""
        // convenient syntax
        Some(1).fmap(combine) <*> Some(2) === Some(3)
      """)
    ),

    slide(
      "Cats Core: Applicative laws",
      scalaC("""
        // homomorphism
        pure(f) <*> pure(x) === pure(f(x))

        // interchange
        ff <*> pure(x) === pure(g => g(x)) <*> ff
      """)
    ),

    slide(
      "Cats Core: Applicative laws",
      scalaC("""
        // composition
        pure(compose) <*> g <*> f <*> x === {
          g <*> f <*> x
        }

        // identity
        pure(identity) <*> x === x
      """)
    ),

    slide(
      "Cats Core: Applicative",
      <.p("It is a Functor, therefore, you can compose it."),
      <.br,
      scalaC("""
        implicit val listOpt = Applicative[List].compose[Option]

        listOpt.ap[Int, Int](List(Some(_ + 1)))(List(Some(1))) === {
          List(Some(2))
        }
      """)
    ),

    noHeaderSlide(
      <.h3("But I need to apply effectful functions to my values"),
      <.br,
      <.h4("Monad")
    ),

    slide(
      "Cats Core: Monad",
      scalaC("""
        def getLine[F[_]](from: String): F[String] = ???

        def parse[F[_]](line: String): F[Person] = ???


        def load[F[_]: Functor](from: String): F[Person] = 
          getLine(from).fmap(a: F[String] => ???)
      """)
    ),

    slide(
      "Cats Core: Monad",
      scalaC("""
        trait Monad[F[_]] extends Applicative[F] {

          def flatMap[A, B](fa: F[A])(f: A => F[B]): F[B]

          ...
        }
      """)
    ),

    slide(
      "Cats Core: Monad",
      scalaC("""
        def getLine[F[_]](from: String): F[String] = ???

        def parse[F[_]](line: String): F[Person] = ???


        def load[F[_]: Monad](from: String): F[Person] = 
          getLine(from).flatMap(a => parse(a))
      """)
    ),

    slide(
      "Cats Core: Monad laws",
      scalaC("""
        // left identity
        pure(a).flatMap(f) == pure(f(a))

        // right identity
        fa.flatMap(pure) === fa

        // associativity
        (pure(a).flatMap(f)).flatMap(g) === {
          pure(a).flatMap(a => f(a).flatMap(g))
        }
      """)
    ),

    exerciseSlide(
      "Let's Code: Core",
      bash("""
        sbt> project typeclasses-incarnations-exercises
        sbt> test:testOnly exercise5.CoreSpec
      """)
    ),

    noHeaderSlide(
      <.h3("What is with Monad composition?"),
      <.br,
      <.h4(
        ^.cls := "fragment fade-in",
        "They aren't composable"
      )
    )
  )

  val monadTransformers = chapter(
    chapterSlide(
      <.h2("Monad Transformers")
    ),

    slide(
      "Monad Transformers",
      <.p("By using Monad Transformers we are able to simulate composition.")
    ),

    slide(
      "Monad Transformers",
      scalaC("""
        // doesn't work
        implicit val listOpt = Monad[List].compose[Option]
      """),
      scalaCFragment("""
        // but
        type ListOpt[A] = List[Option[A]]

        implicit val listOpt = new Monad[ListOpt]] {

          def flatMap[B](f: A => ListOpt[B]]): ListOpt[B]] = 
            value.flatMap { 
              case Some(v) => f(v)
              case None    => List(None)
            }
        }
      """)
    ),

    slide(
      "Monad Transformers",
      <.p("But now we have to implement every combination, which sucks.")
    ),

    slide(
      "Monad Transformers",
      scalaC("""
        type OptionT[F[_], A] = F[Option[A]]

        implicit def optionT[F[_]: Monad] = 
          new Monad[OptionT[F, ?]] {

            def flatMap[A, B](fa: OptionT[F, A])
                             (f: A => OptionT[F, B]): OptionT[F, B] = 
              Monad[F].flatMap(fa) { 
                case Some(v) => f(v)
                case None    => Monad[F].pure(None)
              }
          }
      """)
    ),

    slide(
      "Monad Transformers: kind-projector",
      <.h3("Kind-Projector"),
      <.br,
      scalaC("""
        // OptionT expects two type parameter (F[_, _]), Monad expects F[_]
        // using `?` fixes the type constructor shape
        OptionT[F, ?] ~ ({ type O[A] = OptionT[F, A] })#O
      """)
    ),

    slide(
      "Monad Transformers",
      <.p("Cats provides some data types which can be used as Monad Transformers."),
      <.br,
      <.a(
        ^.href := "https://github.com/typelevel/cats/tree/master/core/src/main/scala/cats/data",
        "cats/core/data"
      )
    ),

    slide(
      "Monad Tranformers",
      scalaC("""
        // our code becomes
        case class OptionT[F, A](value: F[Option[A]]) {

          def flatMap[B](f: A => F[Option[B]])
                        (implicit M: Monad[F]): F[Option[B]] = 
            M.flatMap(value) { 
              case Some(v) => f(v)
              case None    => M.pure(None)
            }

          ...
        }
      """)
    ),

    exerciseSlide(
      "Let's Code: MonadTransformers",
      bash("""
        sbt> project typeclasses-incarnations-exercises
        sbt> test:testOnly exercise5.MonadTransformersSpec
      """)
    ),

    noHeaderSlide(
      <.h3("Last step, some useful Monads")
    )
  )

  val monadIncarnations = chapter(
    chapterSlide(
      <.h2("Monad Incarnations")
    ),

    noHeaderSlide(
      <.h3("Reader Monad")
    ),

    slide(
      "Monads: Reader",
      <.p("Imagine you have some context you have to carry around."),
      <.br,
      scalaC("""
        case class GameConfig(maxMoves: Int)

        def move(dir: Direction, 
                 current: Pos,
                 moveCount: Int,
                 conf: GameConfig): Either[String, Pos] = 
          if (moveCount < conf.maxMoves)
            dir match { ??? }
          else
            Left("No moves left over")
      """)
    ),

    slide(
      "Monads: Reader",
      <.p("But now every function calling `move` has to provide the static `GameConfig`."),
      <.br,
      scalaC("""
        def moveUp(..., conf: GameConfig): Either[String, Pos] = 
          move(Up, current, moveCount, conf)

        ...

        def parse(dirRaw: String, ..., conf: GameConfig): Either[String, Pos] =
          dirRaw.toLowerCase match {
            case "up" => moveUp(current, moveCount, conf)
            ...
          }
      """)
    ),

    slide(
      "Monads: Reader",
      <.p("What if we could store such information in the background?")
    ),

    slide(
      "Monads: Reader",
      scalaC("""
        case class Reader[E, A](run: E => A)

        implicit def readerM[E] = new Monad[Reader[E, ?]] {

          def flatMap[A, B](fa: Reader[E, A])
                           (f: A => Reader[E, B]): Reader[E, B] = {
            Reader { env =>
              val reader = f(fa.run(env))

              reader.run
            }
        }
      """)
    ),

    slide(
      "Monads: Reader",
      scalaC("""
        object Reader {

          // get the context
          def ask[E]: Reader[E, E] = Reader(identity)

          // update context
          def local[E, A](f: E => E)(fa: Reader[E, A]): Reader[E, A] =
            Reader { env =>
              fa.run(f(env))
            }
        }
      """)
    ),

    slide(
      "Monads: Reader",
      scalaC("""
        type ConfR[A] = Reader[GameConfig, A]

        def move(dir: Direction, 
                 current: Pos,
                 moveCount: Int): ConfR[Either[String, Pos]] = 
          Reader.ask[GameConfig].map { conf =>
            if (moveCount < conf.maxMoves)
              dir match { ??? }
            else
              Left("No moves left over")
          }
      """)
    ),

    slide(
      "Monads: Reader",
      scalaC("""
        def moveUp(current: Pos, moveCount: Int): ConfR[Either[String, Pos]] = 
          move(Up, current, moveCount)

        ...

        parse("up", Pos(0, 0), 0).run(GameConfig(5)) === Right(Pos(1, 0))
      """)
    ),

    noHeaderSlide(
      <.h3("What about writing information?")
    ),

    slide(
      "Monads: Writer",
      scalaC("""
        // let's modify the old `moveUp` to log moves
        def moveUp(current: Pos,
                   moveCount: Int,
                   conf: GameConfig,
                   moves: List[String]): Either[String, (List[String], Pos)] =
          moves(Up, current, moveCount, conf)
            .map(pos => ("up" :: moves, pos))
      """)
    ),

    slide(
      "Monads: Writer",
      <.p("But now we carry a list around to store log entries.")
    ),

    slide(
      "Monads: Writer",
      scalaC("""
        case class Writer[E, A](run: (E, A))

        implicit def writerM[E: Semigroup] = new Monad[Writer[E, ?]] {

          def flatMap[B](fa: Writer[E, A])
                        (f: A => Writer[E, B]): Writer[E, B] = {
            val writer = f(fa.run._2)

            (writer.run._1 |+| fa.run._1, writer.run._2)
          }
        }
      """)
    ),

    slide(
      "Monads: Writer",
      scalaC("""
        object Writer {

          // store some information
          def tell[E: Semigroup, A](e: E): Writer[E, A] = 
            Writer { case (before, value) =>
              (e |+| before, value)
            }
        }
      """)
    ),

    slide(
      "Monads: Writer",
      scalaC("""
        type GameW[A] = Writer[List[String], A]

        def moveUp(current: Pos,
                   moveCount: Int,
                   conf: GameConfig): GameW[Either[String, Pos]] =
          for {
            pos <- pure(moves(Up, current, moveCount, conf))
            _   <- Writer.tell(List("up"))
          } yield pos
      """)
    ),

    slide(
      "Monads: Writer",
      scalaC("""
        val (logs, value) = parse("up", Pos(0, 0), 0, GameConfig(5)).run

        logs  === List("up")
        value === Pos(1, 0)
      """)
    ),

    noHeaderSlide(
      <.h3("Can I ask and tell?")
    ),

    slide(
      "Monads: State",
      scalaC("""
        case class State[S, A](run: S => (S, A))

        implicit def stateM[S: Semigroup] = new Monad[State[S, ?]] {

          def flatMap[B](fa: State[S, A])
                        (f: A => State[S, B]): State[S, B] =
            State { state =>
              val (state0, value0) = fa(state)
              val (state1, value1) = f(value0)

              (state1 |+| state0, value1)
            }
        }
      """)
    ),

    slide(
      "Monads: State",
      scalaC("""
        case class GameState(current: Pos, 
                             moveCount: Int)

        type GameS[A] = State[GameState, A]
      """)
    ),

    slide(
      "Monads: State",
      scalaC("""
        def move(dir: Direction, conf: GameConfig): GameS[Either[String, Unit]] = 
          for {
            state  <- State.ask[GameState]
            result <- {
              if (state.moveCount < conf.maxMoves)
                dir match {
                  case Up => State.tell(GameState(Pos(1, 0), 1))
                  ...
                }
              else
                pure(Left("No moves left over"))
            }
          } yield result
      """)
    ),

    slide(
      "Monads: State",
      scalaC("""
        val (state, _) = parse("up", GameConf(5)).run(GameState(Pos(0, 0), 0))

        state === GameState(Pos(1, 0), 1)
      """)
    ),

    noHeaderSlide(
      <.h3("You can do that and more in Cats"),
      <.br,
      <.a(
        ^.href := "https://github.com/typelevel/cats/tree/master/core/src/main/scala/cats/data",
        "cats/core/data"
      )
    )
  )

  val summary = chapter(
    chapterSlide(
      <.h2("Summary")
    ),

    slide(
      "Kernel, Core and Data",
      <.p("We saw useful type classes from Cats kernel, core and data package like:"),
      <.br,
      Enumeration(
        Item.stable("Eq, Order, Semigroup"),
        Item.fadeIn("Functor, Applicative, Monad"),
        Item.fadeIn("Monad Transformers"),
        Item.fadeIn("useful Monad incarnations")
      )
    ),

    noHeaderSlide(
      <.h2("Next Topic"),
      <.br,
      <.h3("Side Effects and IO")
    )
  )

  val Show = ScalaComponent
    .builder[Unit]("Slideshow")
    .renderStatic(
      <.div(
        ^.cls := "reveal",
        <.div(
          ^.cls := "slides",
          overview,
          categorical,
          catsLibrary,
          catsKernel,
          catsCore,
          monadTransformers,
          monadIncarnations,
          summary
        )
      )
    )
    .build

  @JSExport
  def main(): Unit = {
    Show().renderIntoDOM(dom.document.body)
  }
}
