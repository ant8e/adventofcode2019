import java.nio.file.Path

import cats.effect.Blocker
import cats.effect.ExitCode
import cats.effect.IO
import cats.effect.IOApp
import cats.implicits._
import fs2.Stream
import fs2.io
import fs2.text

import scala.annotation.tailrec

object December1 extends IOApp {

  def fuelForMass(mass: Int) = {
    val f = (mass / 3) - 2
    if (f > 0) f else 0
  }

  def fuelForMassCompensated(mass: Int): Int = {
    @tailrec
    def fuelFormMassComp0(acc: Int, mass: Int): Int = {
      val newFuel = fuelForMass(mass)
      if (newFuel == 0) acc
      else fuelFormMassComp0(acc + newFuel, newFuel)
    }
    fuelFormMassComp0(0, mass)
  }

  val path = Path.of(this.getClass.getResource("December1").toURI)

  val dataStream = Stream
    .resource(Blocker[IO])
    .flatMap(
      blocker =>
        io.file
          .readAll[IO](path, blocker, 4096)
          .through(text.utf8Decode)
          .through(text.lines))
    .map(_.toIntOption)
    .collect { case Some(v) => v }

  val fuel= dataStream
    .map(fuelForMass)
    .fold1(_ + _)

  val fuelCompensated = dataStream
    .map(fuelForMassCompensated)
    .fold1(_ + _)

  override def run(args: List[String]): IO[ExitCode] =
    (fuel ++ fuelCompensated).compile.toList
      .flatMap(x => IO(x.foreach(println)))
      .as(ExitCode.Success)
}
