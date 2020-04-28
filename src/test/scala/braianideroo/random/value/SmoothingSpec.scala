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

package braianideroo.random.value

import zio.{Has, ZIO, ZLayer}
import zio.test.Assertion._
import zio.test.{DefaultRunnableSpec, ZSpec, _}

object SmoothingSpec extends DefaultRunnableSpec {

  val probabilities: Map[String, Double] =
    Map("something" -> 1, "pineapple" -> 2, "car" -> 0)

  val probabilitiesLayer
    : ZLayer[Any, Nothing, Has[Probabilities[Any, String]]] =
    ZLayer.fromEffect(for {
      res <- ZIO
        .foreach(probabilities)(
          x => Probability.make[Any](x._2).map(y => (x._1, y))
        )
        .map(_.toMap)
    } yield res)

  val s
    : Spec[Has[Probabilities[Any, String]], TestFailure[Nothing], TestSuccess] =
    suite("Smoothing suite")(
      testM("using noSmoothing doesn't affect the outcome") {
        for {
          probs <- ZIO.access[Has[Probabilities[Any, String]]](_.get)
          _ <- probs.smooth(Smoothing.noSmoothing)
          aux1 = probs.get("something")
          base1 = aux1.get.baseProbability
          prob1 <- aux1.get.probability
          aux2 = probs.get("pineapple")
          base2 = aux2.get.baseProbability
          prob2 <- aux2.get.probability
          aux3 = probs.get("car")
          base3 = aux3.get.baseProbability
          prob3 <- aux3.get.probability
        } yield
          assert(base1)(equalTo(1.0)) &&
            assert(prob1)(equalTo(1.0)) &&
            assert(base2)(equalTo(2.0)) &&
            assert(prob2)(equalTo(2.0)) &&
            assert(base3)(equalTo(0.0)) &&
            assert(prob3)(equalTo(0.0))
      },
      testM("can smooth an iterable of probabilities with prior") {
        for {
          probs <- ZIO.access[Has[Probabilities[Any, String]]](_.get)
          _ <- probs.smooth(Smoothing.priorSmoothing(0.1))
          aux1 = probs.get("something")
          base1 = aux1.get.baseProbability
          prob1 <- aux1.get.probability
          aux2 = probs.get("pineapple")
          base2 = aux2.get.baseProbability
          prob2 <- aux2.get.probability
          aux3 = probs.get("car")
          base3 = aux3.get.baseProbability
          prob3 <- aux3.get.probability
        } yield
          assert(base1)(equalTo(1.0)) &&
            assert(prob1)(equalTo(1.1)) &&
            assert(base2)(equalTo(2.0)) &&
            assert(prob2)(equalTo(2.1)) &&
            assert(base3)(equalTo(0.0)) &&
            assert(prob3)(equalTo(0.1))
      },
      testM(
        "can smooth an iterable of probabilities with good turing " +
          "smoothing"
      ) {
        for {
          probs <- ZIO.access[Has[Probabilities[Any, String]]](_.get)
          _ <- probs.smooth(Smoothing.goodTuringSmoothing)
          aux1 = probs.get("something")
          base1 = aux1.get.baseProbability
          prob1 <- aux1.get.probability
          aux2 = probs.get("pineapple")
          base2 = aux2.get.baseProbability
          prob2 <- aux2.get.probability
          aux3 = probs.get("car")
          base3 = aux3.get.baseProbability
          prob3 <- aux3.get.probability
        } yield
          assert(base1)(equalTo(1.0)) &&
            assert(prob1)(equalTo(1.0)) &&
            assert(base2)(equalTo(2.0)) &&
            assert(prob2)(equalTo(2.0)) &&
            assert(base3)(equalTo(0.0)) &&
            assert(prob3)(equalTo(0.3333333333333333))
      }
    )

  override def spec: ZSpec[_root_.zio.test.environment.TestEnvironment, Any] =
    s.provideLayer(probabilitiesLayer)
}
