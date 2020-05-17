/*
 * Copyright 2020 Braian I. De Roo
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *      http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package braianideroo.random

import braianideroo.random
import zio.{IO, ZIO}

package object value {
  type RandomValue[R, E, A] = ZIO[SeedRandom with R, E, A]
  type RandomVIO[E, A] = RandomValue[Any, E, A]
  type Probabilities[R, A] = Map[A, Probability[R]]
  type SmoothF[R, A] = ZIO[Probabilities[R, A], Nothing, Unit]

  case class Element[R, A](value: A, probability: Probability[R])

  object RandomValue {

    def fromSimpleIterable[A](
      iterable: Iterable[(A, Double)]
    ): RandomVIO[Nothing, Option[A]] =
      for {
        aux <- ZIO.foreach(iterable)(
          x => Probability.make[Any](x._2).map(y => Element(x._1, y))
        )
        res <- fromElementIterable(aux)
      } yield res

    def fromElementIterable[R, A](
      iterable: Iterable[Element[R, A]]
    ): RandomValue[R, Nothing, Option[A]] = {

      def inner(elements: Iterable[Element[R, A]],
                randomValue: Double): ZIO[R, Nothing, Option[A]] =
        elements.headOption match {
          case Some(element) =>
            for {
              prob <- element.probability.probability
              aux <- if (randomValue < prob)
                ZIO.some(element.value)
              else inner(elements.tail, randomValue - prob)
            } yield aux
          case None => ZIO.none
        }

      for {
        total <- ZIO
          .foreach(iterable)(x => x.probability.probability)
          .map(_.sum)
        randomValue <- random.between(0D, total).fold(_ => None, s => Some(s))
        res <- randomValue match {
          case Some(value) => inner(iterable, value)
          case None        => ZIO.none
        }
      } yield res
    }

    def fromIterable[R, A](
      iterable: Iterable[A]
    ): RandomValue[R, Nothing, Option[A]] =
      for {
        elements <- ZIO.foreach(iterable)(
          x =>
            for {
              prob <- Probability.make[R](1)
            } yield Element(x, prob)
        )
        res <- fromElementIterable(elements)
      } yield res

    def fromMap[R, A](
      map: Probabilities[R, A]
    ): RandomValue[R, Nothing, Option[A]] =
      fromElementIterable(map.map(x => Element(x._1, x._2)))
  }

  object Smoothing {
    private def smoothingModifier[R](v: Double): Modifier[R] =
      new Modifier[R] {
        override def value: ZIO[R, Nothing, Option[Double]] =
          ZIO.some(v)
      }

    def noSmoothing[R, A]: SmoothF[R, A] = ZIO.unit

    def priorSmoothing[R, A](prior: Double): SmoothF[R, A] =
      for {
        probabilities <- ZIO.access[Probabilities[R, A]](x => x)
        modifier = smoothingModifier[R](prior)
        _ <- ZIO.foreach(probabilities)(
          x => x._2.addModifier("smoothing", modifier)
        )
      } yield ()

    // it only takes into account base probability, otherwise it would have to
    // recalculate the smoothing for every different R
    def goodTuringSmoothing[R, A]: SmoothF[R, A] =
      for {
        probabilities <- ZIO.access[Probabilities[R, A]](x => x)
        n1 = probabilities.map(x => x._2.baseProbability).count(_ == 1)
        calculated = probabilities.map(x => (x._1, x._2.baseProbability))
        total = calculated.values.sum
        n0 = n1 / total
        modifier0 = smoothingModifier[R](n0)
        _ <- ZIO.foreach_(probabilities)(
          x =>
            if (calculated(x._1) == 0) x._2.addModifier("smoothing", modifier0)
            else ZIO.succeed(())
        )
      } yield ()
  }

  implicit class ProbabilitiesHelper[R, A](value: Probabilities[R, A]) {
    def smooth(smoothF: SmoothF[R, A]): IO[Nothing, Unit] =
      smoothF.provide(value)
  }
}
