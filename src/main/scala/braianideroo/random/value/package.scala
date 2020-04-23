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
  type Probabilities[A] = Map[A, Double]
  type SmoothF[A] = ZIO[Probabilities[A], Nothing, Probabilities[A]]

  case class Probability[A](value: A, probability: Double)

  object RandomValue {

    def fromProbabilityIterable[A](
      iterable: Iterable[Probability[A]]
    ): RandomVIO[Nothing, Option[A]] = {
      val total = iterable.map(_.probability).sum

      @scala.annotation.tailrec
      def inner(probabilities: Iterable[Probability[A]],
                randomValue: Double): Option[A] =
        probabilities.headOption match {
          case Some(probability) =>
            if (randomValue < probability.probability) Some(probability.value)
            else
              inner(probabilities.tail, randomValue - probability.probability)
          case None => None
        }

      for {
        randomValue <- random.between(0D, total).fold(_ => None, s => Some(s))
        res = randomValue match {
          case Some(value) => inner(iterable, value)
          case None        => None
        }
      } yield res
    }

    def fromIterable[A](iterable: Iterable[A]): RandomVIO[Nothing, Option[A]] =
      fromProbabilityIterable(iterable.map(x => Probability(x, 10)))

    def fromMap[A](map: Probabilities[A]): RandomVIO[Nothing, Option[A]] = {
      fromProbabilityIterable(map.map(x => Probability(x._1, x._2)))
    }
  }

  object Smoothing {
    def noSmoothing[A]: SmoothF[A] = ZIO.access[Probabilities[A]](x => x)

    def priorSmoothing[A](prior: Double): SmoothF[A] =
      for {
        probabilities <- ZIO.access[Probabilities[A]](x => x)
        res = probabilities.map(x => (x._1, x._2 + prior))
      } yield res

    def goodTuringSmoothing[A]: SmoothF[A] =
      for {
        probabilities <- ZIO.access[Probabilities[A]](x => x)
        n1 = probabilities.count(_._2 == 1)
        n0 = n1 / probabilities.values.sum
        res = probabilities.map(x => (x._1, if (x._2 == 0) n0 else x._2))
      } yield res
  }

  implicit class ProbabilitiesHelper[A](value: Probabilities[A]) {
    def smooth(smoothF: SmoothF[A]): IO[Nothing, Probabilities[A]] =
      smoothF.provide(value)
  }
}
