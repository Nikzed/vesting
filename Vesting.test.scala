package validator

import scalus.*
import scalus.Compiler.compile
import scalus.builtin.ByteString.*
import scalus.builtin.Data.toData
import scalus.builtin.{ByteString, Data, PlatformSpecific, given}
import scalus.builtin.Data.{FromData, ToData, fromData, toData}
// import scalus.builtin.FromDataInstances.given
import scalus.ledger.api.v1.PubKeyHash
import scalus.ledger.api.v3.*
import scalus.ledger.api.v3.given
import scalus.prelude.*
import scalus.testkit.*
import scalus.uplc.*
import scalus.uplc.eval.*
import scalus.prelude.Option.*
import org.scalatest.funsuite.AnyFunSuite

import scala.language.implicitConversions
import scala.math.Ordering.Implicits.*
import scalus.prelude.*
import scalus.prelude.List
import java.time.LocalDateTime
import scala.compiletime.ops.boolean

class VestingTest extends AnyFunSuite, ScalusTest {
    private val compiled = compile(Vesting.validate)
    private val ownerPKH: PubKeyHash = Mock.mockPubKeyHash(0)
    private val beneficiaryPKH: PubKeyHash = Mock.mockPubKeyHash(1)

    test("Successful spend scenario by owner before timeout") {
        val testCase = TestCase(
          signatories = List(ownerPKH),
          lockUntil = 17000000,
          interval = Interval.always
        )
        val result = checkTestCase(testCase)
        assert(result.isSuccess)
    }

    case class TestCase(
        signatories: List[PubKeyHash],
        lockUntil: BigInt,
        interval: Interval
    )

    def checkTestCase(testCase: TestCase): Result = {
        val inputs = List(
          makePubKeyHashInput(
            hex"1234567890abcdef1234567890abcdef1234567890abcdef12345678",
            0
          )
        )

        val context = ScriptContext(
          txInfo = TxInfo(
            inputs = inputs,
            id = random[TxId],
            signatories = testCase.signatories,
            validRange = testCase.interval
          ),
          scriptInfo = ScriptInfo.SpendingScript(
            txOutRef = inputs.head.outRef,
            datum = Some(
              VestingDatum(
                beneficiary = beneficiaryPKH,
                startTimestamp = 1609459200, // 2021-01-01
                duration = 31536000, // 1 year
                amount = 1000000 // 1
              ).toData
            )
          )
        )

        compiled.runScript(context)
    }
}
