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

import zio.{Has, Task, UIO, ZIO, ZLayer}

package object random {
  type Seed = Long
  type SeedRandom = Has[SeedRandom.Service]
  object SeedRandom {
    trait Service {
      def nextBoolean: UIO[Boolean]
      def nextBytes(n: Int): UIO[Vector[Byte]]
      def nextDouble: UIO[Double]
      def between(minInclusive: Double, maxExclusive: Double): Task[Double]
      def nextFloat: UIO[Float]
      def between(minInclusive: Float, maxExclusive: Float): Task[Float]
      def nextGaussian: UIO[Double]
      def nextInt(): UIO[Int]
      def nextInt(n: Int): Task[Int]
      def between(minInclusive: Int, maxExclusive: Int): Task[Int]
      def nextLong(): UIO[Long]
      def nextLong(n: Long): Task[Long]
      def between(minInclusive: Long, maxExclusive: Long): Task[Long]
      def nextString(length: Int): Task[String]
    }

    object Service {
      val live: ZIO[Has[Seed], Nothing, Service] = for {
        seed <- ZIO.access[Has[Seed]](_.get)
      } yield
        new Service {
          import zio.ZIO._
          val random = new scala.util.Random(seed)

          override def nextBoolean: UIO[Boolean] =
            effectTotal(random.nextBoolean())

          override def nextBytes(n: Int): UIO[Vector[Byte]] =
            effectTotal(random.nextBytes(n).toVector)

          override def nextDouble: UIO[Double] =
            effectTotal(random.nextDouble())

          override def between(minInclusive: Double,
                               maxExclusive: Double): Task[Double] =
            effect(random.between(minInclusive, maxExclusive))

          override def nextFloat: UIO[Float] =
            effectTotal(random.nextFloat)

          override def between(minInclusive: Float,
                               maxExclusive: Float): Task[Float] =
            effect(random.between(minInclusive, maxExclusive))

          override def nextGaussian: UIO[Double] =
            effectTotal(random.nextGaussian())

          override def nextInt(): UIO[Int] =
            effectTotal(random.nextInt())

          override def nextInt(n: Int): Task[Int] =
            effect(random.nextInt(n))

          override def between(minInclusive: Int,
                               maxExclusive: Int): Task[Int] =
            effect(random.between(minInclusive, maxExclusive))

          override def nextLong(): UIO[Long] =
            effectTotal(random.nextLong())

          override def nextLong(n: Long): Task[Long] =
            effect(random.nextLong(n))

          override def between(minInclusive: Long,
                               maxExclusive: Long): Task[Long] =
            effect(random.between(minInclusive, maxExclusive))

          override def nextString(length: Int): Task[String] =
            effect(random.nextString(length))
        }
    }

    val live: ZLayer[Has[Seed], Nothing, Has[Service]] =
      ZLayer.fromEffect(Service.live)
  }

  def nextBoolean: ZIO[SeedRandom, Nothing, Boolean] =
    ZIO.accessM[SeedRandom](_.get.nextBoolean)

  def nextBytes(n: Int): ZIO[SeedRandom, Nothing, Vector[Byte]] =
    ZIO.accessM[SeedRandom](_.get.nextBytes(n))

  def nextDouble: ZIO[SeedRandom, Nothing, Double] =
    ZIO.accessM[SeedRandom](_.get.nextDouble)

  def between(minInclusive: Double,
              maxExclusive: Double): ZIO[SeedRandom, Throwable, Double] =
    ZIO.accessM[SeedRandom](_.get.between(minInclusive, maxExclusive))

  def nextFloat: ZIO[SeedRandom, Nothing, Float] =
    ZIO.accessM[SeedRandom](_.get.nextFloat)

  def between(minInclusive: Float,
              maxExclusive: Float): ZIO[SeedRandom, Throwable, Float] =
    ZIO.accessM[SeedRandom](_.get.between(minInclusive, maxExclusive))

  def nextGaussian: ZIO[SeedRandom, Nothing, Double] =
    ZIO.accessM[SeedRandom](_.get.nextGaussian)

  def nextInt(): ZIO[SeedRandom, Nothing, Int] =
    ZIO.accessM[SeedRandom](_.get.nextInt())

  def nextInt(n: Int): ZIO[SeedRandom, Throwable, Int] =
    ZIO.accessM[SeedRandom](_.get.nextInt(n))

  def between(minInclusive: Int,
              maxExclusive: Int): ZIO[SeedRandom, Throwable, Int] =
    ZIO.accessM[SeedRandom](_.get.between(minInclusive, maxExclusive))

  def nextLong(): ZIO[SeedRandom, Nothing, Long] =
    ZIO.accessM[SeedRandom](_.get.nextLong())

  def nextLong(n: Long): ZIO[SeedRandom, Throwable, Long] =
    ZIO.accessM[SeedRandom](_.get.nextLong(n))

  def between(minInclusive: Long,
              maxExclusive: Long): ZIO[SeedRandom, Throwable, Long] =
    ZIO.accessM[SeedRandom](_.get.between(minInclusive, maxExclusive))

  def nextString(length: Int): ZIO[SeedRandom, Throwable, String] =
    ZIO.accessM[SeedRandom](_.get.nextString(length))
}
