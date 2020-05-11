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

import zio.{Has, Layer, ZLayer}
import zio.test.{DefaultRunnableSpec, ZSpec}
import braianideroo.random
import zio.test._
import zio.test.Assertion._
import zio.test.TestAspect._

object SeedRandomSpec extends DefaultRunnableSpec {

  val seed: Seed = 500000L
  val seedLayer: Layer[Nothing, Has[Seed]] = ZLayer.succeed(seed)

  val seedRandom: ZLayer[Any, Nothing, Has[SeedRandom.Service]] =
    seedLayer >>> braianideroo.random.SeedRandom.live

  val s: Spec[SeedRandom, TestFailure[SeedRandomError], TestSuccess] =
    suite("SeedRandom suite")(
      testM("can generate random boolean values")(
        for {
          res1 <- random.nextBoolean
          res2 <- random.nextBoolean
        } yield
          assert(res1)(equalTo(false)) &&
            assert(res2)(equalTo(false))
      ),
      testM("can generate random bytes")(
        for {
          res1 <- random.nextBytes(5)
          res2 <- random.nextBytes(0)
          res3 <- random.nextBytes(-5)
        } yield
          assert(res1)(
            equalTo(
              Vector(56.toByte, -51.toByte, 2.toByte, 54.toByte, 87.toByte)
            )
          ) &&
            assert(res2)(equalTo(Vector())) &&
            assert(res3)(equalTo(Vector()))
      ),
      testM("can generate doubles")(
        for {
          res1 <- random.nextDouble
          res2 <- random.nextDouble
          res3 <- random.nextDouble
        } yield
          assert(res1)(equalTo(0.21098024283744032D)) &&
            assert(res2)(equalTo(0.06261762275826488D)) &&
            assert(res3)(equalTo(0.28499294361031924D))
      ),
      testM("can generate floats")(
        for {
          res1 <- random.nextFloat
          res2 <- random.nextFloat
          res3 <- random.nextFloat
        } yield
          assert(res1)(equalTo(0.21098024F)) &&
            assert(res2)(equalTo(0.42326474F)) &&
            assert(res3)(equalTo(0.0626176F))
      ),
      testM("can generate gaussians")(
        for {
          res1 <- random.nextGaussian
          res2 <- random.nextGaussian
          res3 <- random.nextGaussian
        } yield
          assert(res1)(equalTo(1.3607498627788064)) &&
            assert(res2)(equalTo(1.496915300390276)) &&
            assert(res3)(equalTo(-0.7178209910871114))
      ),
      testM("can generate ints")(
        for {
          res1 <- random.nextInt
          res2 <- random.nextInt
          res3 <- random.nextInt
        } yield
          assert(res1)(equalTo(906153272)) &&
            assert(res2)(equalTo(1817908311)) &&
            assert(res3)(equalTo(268940608))
      ),
      testM("can generate longs")(
        for {
          res1 <- random.nextLong
          res2 <- random.nextLong
          res3 <- random.nextLong
        } yield
          assert(res1)(equalTo(3891898670221300823L)) &&
            assert(res2)(equalTo(1155091113906380054L)) &&
            assert(res3)(equalTo(5257191817557221304L))
      )
    )

  val s2: Spec[SeedRandom, TestFailure[SeedRandomError], TestSuccess] =
    suite("SeedRandom throwable suite")(
      testM("can generate doubles in a range")(for {
        res1 <- random.between(0D, 0.5D)
      } yield assert(res1)(equalTo(0.10549012141872016D))),
      testM(
        "can't generate doubles if the first value is greater than the second"
      )(for {
        _ <- random.between(0.5D, 0D)
      } yield assert(true)(equalTo(true))) @@ failure,
      testM("can generate floats in a range")(for {
        res1 <- random.between(0F, 0.5F)
      } yield assert(res1)(equalTo(0.10549012F))),
      testM(
        "can't generate floats if the first value is greater than the second"
      )(for {
        _ <- random.between(0.5F, 0F)
      } yield assert(true)(equalTo(true))) @@ failure,
      testM("can generate ints in a range between 0 and N")(for {
        res1 <- random.nextInt(10)
      } yield assert(res1)(equalTo(6))),
      testM("can't generate ints if the bound is equal zero")(for {
        _ <- random.nextInt(0)
      } yield assert(true)(equalTo(true))) @@ failure,
      testM("can't generate ints if the bound is less than zero")(for {
        _ <- random.nextInt(-10)
      } yield assert(true)(equalTo(true))) @@ failure,
      testM("can generate ints in a range")(for {
        res1 <- random.between(4, 70)
      } yield assert(res1)(equalTo(38))),
      testM(
        "can't generate ints if the first value is greater than the second"
      )(for {
        _ <- random.between(10, 0)
      } yield assert(true)(equalTo(true))) @@ failure,
      testM("can generate longs in a range")(for {
        res1 <- random.between(8L, 50L)
      } yield assert(res1)(equalTo(48L))),
      testM(
        "can't generate longs if the first value is greater than the second"
      )(for {
        res1 <- random.between(50L, 2L)
      } yield assert(res1)(equalTo(5L))) @@ failure,
      testM("can generate strings")(for {
        res1 <- random.nextString(5)
      } yield assert(res1)(equalTo("꺞㩢밠囧ㅊ")))
    )

  override def spec: ZSpec[_root_.zio.test.environment.TestEnvironment, Any] =
    suite("all")(s, s2)
      .provideLayer(seedRandom)
}
