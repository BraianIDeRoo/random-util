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

object SmoothingSpec extends DefaultRunnableSpec {

  val probabilities: Probabilities[String] =
    Map("something" -> 1, "pineapple" -> 2, "car" -> 0)

  val s = suite("Smoothing suite")(
    testM("using noSmoothing doesn't affect the outcome") {
      for {
        probs <- ZIO.access[Probabilities[String]](x => x)
        res <- probs.smooth(Smoothing.noSmoothing)
      } yield assert(res)(equalTo(probs))
    },
    testM("can smooth an iterable of probabilities with prior") {
      for {
        probs <- ZIO.access[Probabilities[String]](x => x)
        res <- probs.smooth(Smoothing.priorSmoothing(0.1))
      } yield
        assert(res.get("something"))(equalTo(Some(1.1))) &&
          assert(res.get("pineapple"))(equalTo(Some(2.1))) &&
          assert(res.get("car"))(equalTo(Some(0.1)))
    },
    testM(
      "can smooth an iterable of probabilities with good turing " +
        "smoothing"
    ) {
      for {
        probs <- ZIO.access[Probabilities[String]](x => x)
        res <- probs.smooth(Smoothing.goodTuringSmoothing)
      } yield
        assert(res)(
          equalTo(
            Map(
              "something" -> 1.0,
              "pineapple" -> 2.0,
              "car" -> 0.3333333333333333
            )
          )
        )
    }
  )

  override def spec: ZSpec[_root_.zio.test.environment.TestEnvironment, Any] =
    s.provide(probabilities)
}
