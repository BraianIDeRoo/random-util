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

object ProbabilitySpec extends DefaultRunnableSpec {

  val s: Spec[Any, TestFailure[Nothing], TestSuccess] =
    suite("probability suite")(
      testM("can create a probability")(
        for {
          prob <- Probability.make[Int](0.5)
          defaultProb = prob.baseProbability
          modifiers <- prob.modifiers
        } yield
          assert(defaultProb)(equalTo(0.5)) &&
            assert(modifiers.size)(equalTo(0))
      ),
      testM("can add a modifier")(
        for {
          prob <- Probability.make[Int](0.5)
          modifier = Modifier[Int](ZIO.succeed(Some(1)))
          _ <- prob.addModifier("test", modifier)
          testModList <- prob.modifiers("test")
          modList <- prob.modifiers
        } yield
          assert(testModList)(isSome(equalTo(List(modifier)))) &&
            assert(modList.contains("test"))(equalTo(true)) &&
            assert(modList("test"))(equalTo(List(modifier)))
      ),
      testM("can remove a modifier")(
        for {
          prob <- Probability.make[Int](0.5)
          modifier = Modifier[Int](ZIO.succeed(Some(1)))
          _ <- prob.addModifier("test", modifier)
          bool <- prob.removeModifier(modifier)
          testModList <- prob.modifiers("test")
          modList <- prob.modifiers
        } yield
          assert(bool)(equalTo(true)) &&
            assert(testModList)(isSome(equalTo(List()))) &&
            assert(modList("test"))(equalTo(List()))
      ),
      testM("can remove modifiers with a modifier")(
        for {
          prob <- Probability.make[Int](0.5)
          modifier1 = Modifier[Int](ZIO.succeed(Some(1)))
          modifier2 = Modifier[Int](ZIO.succeed(Some(2)))
          _ <- prob.addModifier("test", modifier1)
          _ <- prob.addModifier("test", modifier2)
          bool <- prob.removeModifier("test")
          testModList <- prob.modifiers("test")
          modList <- prob.modifiers
        } yield
          assert(bool)(equalTo(true)) &&
            assert(testModList)(isNone) &&
            assert(modList.get("test"))(isNone)
      )
    )

  override def spec: ZSpec[_root_.zio.test.environment.TestEnvironment, Any] =
    s
}
