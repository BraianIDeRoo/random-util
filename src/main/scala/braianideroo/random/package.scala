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

package braianideroo

import zio._

import scala.util.Random

package object random {
  type Seed = Long
  type SeedRandom = Has[SeedRandom.Service]

  sealed trait SeedRandomError
  case class InvalidSeedId(seedId: Int, maxValidId: Int) extends SeedRandomError
  case class ThrowableError(throwable: Throwable) extends SeedRandomError

  object SeedRandom {
    trait Service {
      def addSeed(seed: Long): UIO[Int]

      def nextBoolean: IO[SeedRandomError, Boolean]
      def nextBooleanWithSeed(seedId: Int): IO[SeedRandomError, Boolean]

      def nextBytes(n: Int): IO[SeedRandomError, Vector[Byte]]
      def nextBytesWithSeed(n: Int,
                            seedId: Int): IO[SeedRandomError, Vector[Byte]]

      def nextDouble: IO[SeedRandomError, Double]
      def nextDoubleWithSeed(seedId: Int): IO[SeedRandomError, Double]

      def between(minInclusive: Double,
                  maxExclusive: Double): IO[SeedRandomError, Double]
      def betweenWithSeed(minInclusive: Double,
                          maxExclusive: Double,
                          seedId: Int): IO[SeedRandomError, Double]

      def nextFloat: IO[SeedRandomError, Float]
      def nextFloatWithSeed(seedId: Int): IO[SeedRandomError, Float]

      def between(minInclusive: Float,
                  maxExclusive: Float): IO[SeedRandomError, Float]
      def betweenWithSeed(minInclusive: Float,
                          maxExclusive: Float,
                          seedId: Int): IO[SeedRandomError, Float]

      def nextGaussian: IO[SeedRandomError, Double]
      def nextGaussianWithSeed(seedId: Int): IO[SeedRandomError, Double]

      def nextInt: IO[SeedRandomError, Int]
      def nextIntWithSeed(seedId: Int): IO[SeedRandomError, Int]

      def nextInt(n: Int): IO[SeedRandomError, Int]
      def nextIntWithSeed(n: Int, seedId: Int): IO[SeedRandomError, Int]

      def between(minInclusive: Int,
                  maxExclusive: Int): IO[SeedRandomError, Int]
      def betweenWithSeed(minInclusive: Int,
                          maxExclusive: Int,
                          seedId: Int): IO[SeedRandomError, Int]

      def nextLong: IO[SeedRandomError, Long]
      def nextLongWithSeed(seedId: Int): IO[SeedRandomError, Long]

      def nextLong(n: Long): IO[SeedRandomError, Long]
      def nextLongWithSeed(n: Long, seedId: Int): IO[SeedRandomError, Long]

      def between(minInclusive: Long,
                  maxExclusive: Long): IO[SeedRandomError, Long]
      def betweenWithSeed(minInclusive: Long,
                          maxExclusive: Long,
                          seedId: Int): IO[SeedRandomError, Long]

      def nextString(length: Int): IO[SeedRandomError, String]
      def nextStringWithSeed(length: Int,
                             seedId: Int): IO[SeedRandomError, String]
    }

    object Service {
      val live: ZIO[Has[Seed], Nothing, Service] = for {
        seed <- ZIO.access[Has[Seed]](_.get)
        randoms <- Ref.make[Vector[Random]](Vector(new scala.util.Random(seed)))
      } yield
        new Service {
          import zio.ZIO._

          private def withRandomAux[A](
            seedId: Int,
            action: Random => IO[SeedRandomError, A]
          ): ZIO[Any, SeedRandomError, A] =
            for {
              rands <- randoms.get
              value <- rands.lift(seedId) match {
                case Some(r) => action(r)
                case None    => fail(InvalidSeedId(seedId, rands.size - 1))
              }
            } yield value

          private def withRandom[A](
            seedId: Int,
            action: Random => A
          ): ZIO[Any, SeedRandomError, A] =
            withRandomAux(seedId, x => effectTotal(action(x)))

          private def withRandomTask[A](
            seedId: Int,
            action: Random => A
          ): ZIO[Any, SeedRandomError, A] =
            withRandomAux(
              seedId,
              x => effect(action(x)).mapError(t => ThrowableError(t))
            )

          override def addSeed(seed: Seed): UIO[Int] =
            for {
              rands <- randoms.get
              randomLength = rands.size
              _ <- randoms.update(_ :+ new scala.util.Random(seed))
            } yield randomLength

          override def nextBoolean: IO[SeedRandomError, Boolean] =
            nextBooleanWithSeed(0)

          override def nextBooleanWithSeed(
            seedId: Int
          ): IO[SeedRandomError, Boolean] =
            withRandom(seedId, _.nextBoolean())

          override def nextBytes(n: Int): IO[SeedRandomError, Vector[Byte]] =
            nextBytesWithSeed(n, 0)

          override def nextBytesWithSeed(
            n: Int,
            seedId: Int
          ): IO[SeedRandomError, Vector[Byte]] =
            withRandom(seedId, _.nextBytes(n).toVector)

          override def nextDouble: IO[SeedRandomError, Double] =
            nextDoubleWithSeed(0)

          override def nextDoubleWithSeed(
            seedId: Int
          ): IO[SeedRandomError, Double] =
            withRandom(seedId, _.nextDouble())

          override def between(
            minInclusive: Double,
            maxExclusive: Double
          ): IO[SeedRandomError, Double] =
            betweenWithSeed(minInclusive, maxExclusive, 0)

          override def betweenWithSeed(
            minInclusive: Double,
            maxExclusive: Double,
            seedId: Int
          ): IO[SeedRandomError, Double] =
            withRandomTask(seedId, _.between(minInclusive, maxExclusive))

          override def nextFloat: IO[SeedRandomError, Float] =
            nextFloatWithSeed(0)

          override def nextFloatWithSeed(
            seedId: Int
          ): IO[SeedRandomError, Float] =
            withRandom(seedId, _.nextFloat())

          override def between(
            minInclusive: Float,
            maxExclusive: Float
          ): IO[SeedRandomError, Float] =
            betweenWithSeed(minInclusive, maxExclusive, 0)

          override def betweenWithSeed(
            minInclusive: Float,
            maxExclusive: Float,
            seedId: Int
          ): IO[SeedRandomError, Float] =
            withRandomTask(seedId, _.between(minInclusive, maxExclusive))

          override def nextGaussian: IO[SeedRandomError, Double] =
            nextGaussianWithSeed(0)

          override def nextGaussianWithSeed(
            seedId: Int
          ): IO[SeedRandomError, Double] =
            withRandom(seedId, _.nextGaussian())

          override def nextInt: IO[SeedRandomError, Int] =
            nextIntWithSeed(0)

          override def nextIntWithSeed(seedId: Int): IO[SeedRandomError, Int] =
            withRandom(seedId, _.nextInt())

          override def nextInt(n: Int): IO[SeedRandomError, Int] =
            nextIntWithSeed(n, 0)

          override def nextIntWithSeed(n: Int,
                                       seedId: Int): IO[SeedRandomError, Int] =
            withRandomTask(seedId, _.nextInt(n))

          override def between(minInclusive: Int,
                               maxExclusive: Int): IO[SeedRandomError, Int] =
            betweenWithSeed(minInclusive, maxExclusive, 0)

          override def betweenWithSeed(minInclusive: Int,
                                       maxExclusive: Int,
                                       seedId: Int): IO[SeedRandomError, Int] =
            withRandomTask(seedId, _.between(minInclusive, maxExclusive))

          override def nextLong: IO[SeedRandomError, Seed] =
            nextLongWithSeed(0)

          override def nextLongWithSeed(
            seedId: Int
          ): IO[SeedRandomError, Seed] =
            withRandom(seedId, _.nextLong())

          override def nextLong(n: Seed): IO[SeedRandomError, Seed] =
            nextLongWithSeed(n, 0)

          override def nextLongWithSeed(
            n: Seed,
            seedId: Int
          ): IO[SeedRandomError, Seed] =
            withRandomTask(seedId, _.nextLong(n))

          override def between(minInclusive: Seed,
                               maxExclusive: Seed): IO[SeedRandomError, Seed] =
            betweenWithSeed(minInclusive, maxExclusive, 0)

          override def betweenWithSeed(minInclusive: Seed,
                                       maxExclusive: Seed,
                                       seedId: Int): IO[SeedRandomError, Seed] =
            withRandomTask(seedId, _.between(minInclusive, maxExclusive))

          override def nextString(length: Int): IO[SeedRandomError, String] =
            nextStringWithSeed(length, 0)

          override def nextStringWithSeed(
            length: Int,
            seedId: Int
          ): IO[SeedRandomError, String] =
            withRandomTask(seedId, _.nextString(length))
        }
    }

    val live: ZLayer[Has[Seed], Nothing, Has[Service]] =
      ZLayer.fromEffect(Service.live)
  }

  def addSeed(seed: Long): URIO[SeedRandom, Int] =
    ZIO.accessM(_.get.addSeed(seed))

  def nextBoolean: ZIO[SeedRandom, SeedRandomError, Boolean] =
    ZIO.accessM(_.get.nextBoolean)
  def nextBooleanWithSeed(
    seedId: Int
  ): ZIO[SeedRandom, SeedRandomError, Boolean] =
    ZIO.accessM(_.get.nextBooleanWithSeed(seedId))

  def nextBytes(n: Int): ZIO[SeedRandom, SeedRandomError, Vector[Byte]] =
    ZIO.accessM(_.get.nextBytes(n))
  def nextBytesWithSeed(
    n: Int,
    seedId: Int
  ): ZIO[SeedRandom, SeedRandomError, Vector[Byte]] =
    ZIO.accessM(_.get.nextBytesWithSeed(n, seedId))

  def nextDouble: ZIO[SeedRandom, SeedRandomError, Double] =
    ZIO.accessM(_.get.nextDouble)
  def nextDoubleWithSeed(
    seedId: Int
  ): ZIO[SeedRandom, SeedRandomError, Double] =
    ZIO.accessM(_.get.nextDoubleWithSeed(seedId))

  def between(minInclusive: Double,
              maxExclusive: Double): ZIO[SeedRandom, SeedRandomError, Double] =
    ZIO.accessM(_.get.between(minInclusive, maxExclusive))
  def betweenWithSeed(minInclusive: Double,
                      maxExclusive: Double,
                      seedId: Int): ZIO[SeedRandom, SeedRandomError, Double] =
    ZIO.accessM(_.get.betweenWithSeed(minInclusive, maxExclusive, seedId))

  def nextFloat: ZIO[SeedRandom, SeedRandomError, Float] =
    ZIO.accessM(_.get.nextFloat)
  def nextFloatWithSeed(seedId: Int): ZIO[SeedRandom, SeedRandomError, Float] =
    ZIO.accessM(_.get.nextFloatWithSeed(seedId))

  def between(minInclusive: Float,
              maxExclusive: Float): ZIO[SeedRandom, SeedRandomError, Float] =
    ZIO.accessM(_.get.between(minInclusive, maxExclusive))
  def betweenWithSeed(minInclusive: Float,
                      maxExclusive: Float,
                      seedId: Int): ZIO[SeedRandom, SeedRandomError, Float] =
    ZIO.accessM(_.get.betweenWithSeed(minInclusive, maxExclusive, seedId))

  def nextGaussian: ZIO[SeedRandom, SeedRandomError, Double] =
    ZIO.accessM(_.get.nextGaussian)
  def nextGaussianWithSeed(
    seedId: Int
  ): ZIO[SeedRandom, SeedRandomError, Double] =
    ZIO.accessM(_.get.nextGaussianWithSeed(seedId))

  def nextInt: ZIO[SeedRandom, SeedRandomError, Int] =
    ZIO.accessM(_.get.nextInt)
  def nextIntWithSeed(seedId: Int): ZIO[SeedRandom, SeedRandomError, Int] =
    ZIO.accessM(_.get.nextIntWithSeed(seedId))

  def nextInt(n: Int): ZIO[SeedRandom, SeedRandomError, Int] =
    ZIO.accessM(_.get.nextInt(n))
  def nextIntWithSeed(n: Int,
                      seedId: Int): ZIO[SeedRandom, SeedRandomError, Int] =
    ZIO.accessM(_.get.nextIntWithSeed(n, seedId))

  def between(minInclusive: Int,
              maxExclusive: Int): ZIO[SeedRandom, SeedRandomError, Int] =
    ZIO.accessM(_.get.between(minInclusive, maxExclusive))
  def betweenWithSeed(minInclusive: Int,
                      maxExclusive: Int,
                      seedId: Int): ZIO[SeedRandom, SeedRandomError, Int] =
    ZIO.accessM(_.get.betweenWithSeed(minInclusive, maxExclusive, seedId))

  def nextLong: ZIO[SeedRandom, SeedRandomError, Long] =
    ZIO.accessM(_.get.nextLong)
  def nextLongWithSeed(seedId: Int): ZIO[SeedRandom, SeedRandomError, Long] =
    ZIO.accessM(_.get.nextLongWithSeed(seedId))

  def nextLong(n: Long): ZIO[SeedRandom, SeedRandomError, Long] =
    ZIO.accessM(_.get.nextLong(n))
  def nextLongWithSeed(n: Long,
                       seedId: Int): ZIO[SeedRandom, SeedRandomError, Long] =
    ZIO.accessM(_.get.nextLongWithSeed(n, seedId))

  def between(minInclusive: Long,
              maxExclusive: Long): ZIO[SeedRandom, SeedRandomError, Long] =
    ZIO.accessM(_.get.between(minInclusive, maxExclusive))
  def betweenWithSeed(minInclusive: Long,
                      maxExclusive: Long,
                      seedId: Int): ZIO[SeedRandom, SeedRandomError, Long] =
    ZIO.accessM(_.get.betweenWithSeed(minInclusive, maxExclusive, seedId))

  def nextString(length: Int): ZIO[SeedRandom, SeedRandomError, String] =
    ZIO.accessM(_.get.nextString(length))
  def nextStringWithSeed(
    length: Int,
    seedId: Int
  ): ZIO[SeedRandom, SeedRandomError, String] =
    ZIO.accessM(_.get.nextStringWithSeed(length, seedId))
}
