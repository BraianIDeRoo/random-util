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

import zio.{Has, Layer, ZIO, ZLayer}
import zio.test.{DefaultRunnableSpec, ZSpec}
import braianideroo.random.{SeedRandom, SeedRandomSpec}
import zio.test._
import zio.test.Assertion._

object RandomValueSpec extends DefaultRunnableSpec {

  val randomLayer: ZLayer[Any, Nothing, Has[SeedRandom.Service]] =
    SeedRandomSpec.seedRandom

  val probabilities: List[Probability[String]] =
    List(Probability("A", 1), Probability("B", 4), Probability("C", 6))
  val probabilityLayer: Layer[Nothing, Has[List[Probability[String]]]] =
    ZLayer.succeed(probabilities)

  val values: List[String] = List("A", "B", "C")
  val valuesLayer: Layer[Nothing, Has[List[String]]] = ZLayer.succeed(values)

  val s: Spec[SeedRandom with Any with Has[List[Probability[String]]] with Has[
    List[String]
  ], TestFailure[Nothing], TestSuccess] = suite("RandomValue suite")(
    testM("can randomly select from a list of probabilities")(
      for {
        list <- ZIO.access[Has[List[Probability[String]]]](_.get)
        res1 <- RandomValue.fromProbabilityIterable(list)
        res2 <- RandomValue.fromProbabilityIterable(list)
        res3 <- RandomValue.fromProbabilityIterable(list)
      } yield
        assert(res1)(equalTo(Some("B"))) &&
          assert(res2)(equalTo(Some("A"))) &&
          assert(res3)(equalTo(Some("B")))
    ),
    testM("can randomly select from a list of elements")(
      for {
        list <- ZIO.access[Has[List[String]]](_.get)
        res1 <- RandomValue.fromIterable(list)
        res2 <- RandomValue.fromIterable(list)
        res3 <- RandomValue.fromIterable(list)
        res4 <- RandomValue.fromIterable(list)
      } yield
        assert(res1)(equalTo(Some("A"))) &&
          assert(res2)(equalTo(Some("A"))) &&
          assert(res3)(equalTo(Some("A"))) &&
          assert(res4)(equalTo(Some("A")))
    )
  )

  override def spec: ZSpec[_root_.zio.test.environment.TestEnvironment, Any] =
    s.provideLayer(randomLayer ++ probabilityLayer ++ valuesLayer)
}
