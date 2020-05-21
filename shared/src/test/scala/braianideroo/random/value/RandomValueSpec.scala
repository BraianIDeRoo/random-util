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

  val probabilities: List[(String, Int)] =
    List(("A", 1), ("B", 4), ("C", 6))
  val probabilityLayer: Layer[Nothing, Has[Probabilities[Any, String]]] =
    ZLayer.fromEffect {
      for {
        res <- ZIO
          .foreach(probabilities)(
            x => Probability.make[Any](x._2).map(y => (x._1, y))
          )
          .map(_.toMap)
      } yield res
    }

  val values: List[String] = List("A", "B", "C")
  val valuesLayer: Layer[Nothing, Has[List[String]]] = ZLayer.succeed(values)

  val s: Spec[SeedRandom with Any with Has[Probabilities[Any, String]] with Has[
    List[String]
  ], TestFailure[Nothing], TestSuccess] = suite("RandomValue suite")(
    testM("can randomly select from a list of probabilities")(
      for {
        map <- ZIO.access[Has[Probabilities[Any, String]]](_.get)
        res1 <- RandomValue.fromMap(map)
        res2 <- RandomValue.fromMap(map)
        res3 <- RandomValue.fromMap(map)
      } yield
        assert(res1)(equalTo(Some("B"))) &&
          assert(res2)(equalTo(Some("A"))) &&
          assert(res3)(equalTo(Some("B")))
    ),
    testM("can randomly select from a list of elements")(
      for {
        list <- ZIO.access[Has[List[String]]](_.get)
        res1 <- RandomValue.fromIterable[Any, String](list)
        res2 <- RandomValue.fromIterable[Any, String](list)
        res3 <- RandomValue.fromIterable[Any, String](list)
        res4 <- RandomValue.fromIterable[Any, String](list)
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
