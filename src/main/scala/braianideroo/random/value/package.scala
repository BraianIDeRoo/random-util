package braianideroo.random

import braianideroo.random
import zio.ZIO

package object value {
  type RandomValue[R, E, A] = ZIO[SeedRandom with R, E, A]
  type RandomVIO[E, A] = RandomValue[Any, E, A]

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
  }
}
