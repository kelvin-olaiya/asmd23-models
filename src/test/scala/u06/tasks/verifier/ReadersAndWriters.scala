package u06.tasks.verifier

import org.scalacheck.Properties
import u06.modelling.PetriNet.*
import u06.modelling.PetriNet
import u06.utils.MSet
import u06.modelling.SystemAnalysis.Path
import org.scalacheck.Prop.forAll
import org.scalacheck.Arbitrary.arbitrary
import org.scalacheck.Arbitrary
import u06.modelling.SystemAnalysis.{paths, pathsUpToDepth, completePathsUpToDepth}
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers
import org.scalacheck.Gen
import scala.annotation.init

object TestSetup:
  enum RWState:
    case IDLE, R, W, RC, WC, DONE
  import RWState.*

  val readersAndWritersPN = PetriNet[RWState](
    MSet(IDLE) ~~> MSet(R),
    MSet(IDLE) ~~> MSet(W),
    MSet(R) ~~> MSet(RC) ^^^ MSet(WC),
    MSet(W) ~~> MSet(WC) ^^^ MSet(WC),
    MSet(W) ~~> MSet(WC) ^^^ MSet(RC),
    MSet(RC) ~~> MSet(DONE),
    MSet(WC) ~~> MSet(DONE)
  ).toSystem

object ReadersAndWriters extends Properties("Readers & Writers"):
  import TestSetup.*
  import RWState.*

  extension (path: Path[Marking[RWState]])
    def hasMutualExclusion: Boolean = path.forall { m =>
      !((m matches MSet(RC, WC)) && !(m matches MSet(WC, WC)))
    }

  val markingGenerator =
    for
      n <- Gen.choose(1, 10)
      states <- Gen.listOfN(n, Gen.const(IDLE))
    yield MSet(states*)

  def pathsGenerator(length: Int = 10): Gen[Path[Marking[RWState]]] =
    for
      initialMarking <- markingGenerator
      p <- Gen.oneOf(readersAndWritersPN.pathsUpToDepth(initialMarking, length))
    yield p

  given Arbitrary[Path[Marking[RWState]]] = Arbitrary(pathsGenerator(60))

  property("In no path long at most 100 states mutual exclusion fails") =
    forAll: (path: Path[Marking[RWState]]) =>
      path.hasMutualExclusion
