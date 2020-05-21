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

import java.io._
import java.nio.ByteBuffer
import zio.{Ref, ZIO}

class Probability[R] private (val baseProbability: Double,
                              _modifiers: Ref[Map[String, List[Modifier[R]]]]) {
  def probability: ZIO[R, Nothing, Double] =
    for {
      env <- ZIO.access[R](x => x)
      mods <- _modifiers.get
      p = mods.values.flatten
      aux <- ZIO.foreach(p)(x => x.value.provide(env))
      res = baseProbability + aux.flatten.sum
    } yield res

  def modifiers: ZIO[Any, Nothing, Map[String, List[Modifier[R]]]] =
    _modifiers.get

  def modifiers(
    identifier: String
  ): ZIO[Any, Nothing, Option[List[Modifier[R]]]] =
    _modifiers.get.map(_.get(identifier))

  def addModifier(identifier: String,
                  modifier: Modifier[R]): ZIO[Any, Nothing, Unit] =
    for {
      mods <- modifiers
      contains = mods.exists(_._2.contains(modifier))
      _ <- if (contains) ZIO.succeed(false)
      else
        _modifiers.update(
          map =>
            map.updated(identifier, map.get(identifier) match {
              case Some(modifierList) => modifierList :+ modifier
              case None               => List(modifier)
            })
        ) *> ZIO.succeed(true)
    } yield ()

  def removeModifier(modifier: Modifier[R]): ZIO[Any, Nothing, Boolean] =
    for {
      mods <- modifiers
      contains = mods.exists(_._2.contains(modifier))
      res <- if (contains)
        _modifiers.update(
          map => map.map(x => (x._1, x._2.filterNot(_ == modifier)))
        ) *> ZIO.succeed(true)
      else ZIO.succeed(false)
    } yield res

  def removeModifier(identifier: String): ZIO[Any, Nothing, Boolean] =
    for {
      mods <- modifiers
      contains = mods.exists(_._1 == identifier)
      res <- if (contains)
        _modifiers.update(map => map.removed(identifier)) *> ZIO.succeed(true)
      else ZIO.succeed(false)
    } yield res

  def toBytes: ZIO[Any, Nothing, Array[Byte]] = {
    val bb = ByteBuffer.allocate(64)
    val probabilityArray = bb.putDouble(baseProbability).array()
    val arrStream = new ByteArrayOutputStream()
    val oos = new ObjectOutputStream(arrStream)
    oos.writeObject()
    for {
      mods <- _modifiers.get
      _ = oos.writeObject(mods)
      _ = oos.flush()
      modArray = arrStream.toByteArray
    } yield probabilityArray ++ modArray
  }
}

object Probability {

  sealed trait ProbabilityDeserializationError
  case object InvalidSeqSize extends ProbabilityDeserializationError
  case class ThrowableError(throwable: Throwable)
      extends ProbabilityDeserializationError

  def fromByteArray[R](
    bytes: Seq[Byte]
  ): ZIO[Any, ProbabilityDeserializationError, Probability[R]] = {
    if (bytes.length >= 64) {
      val defProbabilityArr = bytes.slice(0, 64).toArray
      val probability: Double = ByteBuffer.wrap(defProbabilityArr).getDouble()

      (for {
        modArray <- ZIO.effect(bytes.slice(64, bytes.length).toArray)
        bis = new ByteArrayInputStream(modArray)
        in = new ObjectInputStream(bis)
        mods <- ZIO.effect(
          in.readObject().asInstanceOf[Map[String, List[Modifier[R]]]]
        )
        res <- make(probability, mods)
      } yield res).mapError(f => ThrowableError(f))

    } else ZIO.fail(InvalidSeqSize)
  }

  def make[R](
    baseProbability: Double,
    modifiers: Map[String, List[Modifier[R]]]
  ): ZIO[Any, Nothing, Probability[R]] =
    for {
      modifiers <- Ref.make[Map[String, List[Modifier[R]]]](Map())
    } yield new Probability[R](baseProbability, modifiers)

  def make[R](baseProbability: Double): ZIO[Any, Nothing, Probability[R]] =
    make(baseProbability, Map())
}
