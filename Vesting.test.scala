package validator

import scalus.*
import scalus.Compiler.compile
import scalus.builtin.ByteString.*
import scalus.builtin.Data.toData
import scalus.builtin.{ByteString, Data, PlatformSpecific, given}
import scalus.builtin.Data.{FromData, ToData, fromData, toData}
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
import scalus.ledger.api.v1.Value.getLovelace
import scalus.ledger.api.v1.Address
import scalus.ledger.api.v1.Credential.{PubKeyCredential, ScriptCredential}
import scalus.ledger.api.v2.OutputDatum
import scalus.ledger.api.v1.IntervalBoundType.*

class VestingTest extends AnyFunSuite, ScalusTest {
    private val ownerPKH: PubKeyHash = Mock.mockPubKeyHash(0)
    private val beneficiaryPKH: PubKeyHash = Mock.mockPubKeyHash(1)

    private val defaultStartTime: PosixTime = BigInt(1609459200000L)
    private val defaultDuration: PosixTime = BigInt(31536000000L)
    private val defaultInitialAmount: Lovelace = BigInt(20_000_000L)

    case class TestCase(
        signatories: List[PubKeyHash],
        interval: Interval,
        vestingDatum: VestingDatum,
        redeemer: VestingRedeemer
    )

    def checkTestCase(testCase: TestCase): Result = {
        val vestingDatum = testCase.vestingDatum
        val redeemer = testCase.redeemer
        val signatories = testCase.signatories
        val interval = testCase.interval

        val inputs = List(
          makePubKeyHashInput(
            ownerPKH.hash,
            vestingDatum.amount // If there is an overpay, it could be released by the beneficiary?? (remove validRange)
            // 100_000_000L
          )
        )

        val amountToWithdraw = vestingDatum.amount
        val outputs = List(
          makePubKeyHashOutput(
            beneficiaryPKH.hash,
            amountToWithdraw
          ),
          // as I get this, we should return the rest to the owner if something happenes, should I leave 0 then??
          TxOut(
            address = Address(PubKeyCredential(PubKeyHash(ownerPKH.hash)), Option.None),
            value = Value.lovelace(0),
            datum = OutputDatum.OutputDatum(vestingDatum.toData)
          )
        )

        val txInfo = TxInfo(
          inputs = inputs,
          id = random[TxId],
          signatories = signatories,
          outputs = outputs,
          validRange = interval
        )

        val scriptContext = ScriptContext(
          txInfo = txInfo,
          redeemer = toData(redeemer),
          scriptInfo = ScriptInfo.SpendingScript(
            txOutRef = inputs.head.outRef,
            datum = Some(vestingDatum.toData)
          )
        )

        // printAdaInInputs(txInfo, vestingDatum, redeemer)
        // printContractOutput(txInfo)
        // printAvailableAmout(txInfo, vestingDatum)

        VestingScript.compiled.runScript(scriptContext)
    }

    test("Successful full withdrawal at/after vesting period ends") {
        val vestingDatum = VestingDatum(
          beneficiary = beneficiaryPKH,
          startTimestamp = defaultStartTime,
          duration = defaultDuration,
          amount = defaultInitialAmount
        )
        val signatories = List(beneficiaryPKH, ownerPKH)
        val interval = Interval.after(vestingDatum.startTimestamp + vestingDatum.duration)
        val redeemer = VestingRedeemer(vestingDatum.amount)

        val result = checkTestCase(
          TestCase(
            signatories = signatories,
            interval = interval,
            vestingDatum = vestingDatum,
            redeemer = redeemer
          )
        )

        println(result)
        assert(result.isSuccess, "Script execution should succeed")
    }

    test("Fail full withdrawal before vesting period ends") {
        val vestingDatum = VestingDatum(
          beneficiary = beneficiaryPKH,
          startTimestamp = defaultStartTime,
          duration = defaultDuration,
          amount = defaultInitialAmount
        )
        val signatories = List(beneficiaryPKH, ownerPKH)
        val interval = Interval.after(vestingDatum.startTimestamp)
        val redeemer = VestingRedeemer(vestingDatum.amount)

        val result = checkTestCase(
          TestCase(
            signatories = signatories,
            interval = interval,
            vestingDatum = vestingDatum,
            redeemer = redeemer
          )
        )

        println(result)
        assert(result.isFailure, "Script execution should fail before the vesting period ends")
    }

    test("2 Script Contexts") {
        val vestingDatum = VestingDatum(
          beneficiary = beneficiaryPKH,
          startTimestamp = defaultStartTime,
          duration = defaultDuration,
          amount = defaultInitialAmount
        )
        val signatories = List(beneficiaryPKH, ownerPKH)
        val interval = Interval.after(vestingDatum.startTimestamp + vestingDatum.duration)
        val redeemer = VestingRedeemer(vestingDatum.amount)

        val inputs = List(
          makePubKeyHashInput(
            ownerPKH.hash,
            vestingDatum.amount
          )
        )

        val amountToWithdraw = vestingDatum.amount
        val outputs = List(
          makePubKeyHashOutput(
            beneficiaryPKH.hash,
            amountToWithdraw
          ),
          TxOut(
            address = Address(PubKeyCredential(PubKeyHash(ownerPKH.hash)), Option.None),
            value = Value.lovelace(0),
            datum = OutputDatum.OutputDatum(vestingDatum.toData)
          )
        )

        val txInfo = TxInfo(
          inputs = inputs,
          id = random[TxId],
          signatories = signatories,
          outputs = outputs,
          validRange = interval
        )

        val scriptContext = ScriptContext(
          txInfo = txInfo,
          redeemer = toData(redeemer),
          scriptInfo = ScriptInfo.SpendingScript(
            txOutRef = inputs.head.outRef,
            datum = Some(vestingDatum.toData)
          )
        )
        val scriptContext2 = ScriptContext(
          txInfo = txInfo,
          redeemer = toData(redeemer),
          scriptInfo = ScriptInfo.SpendingScript(
            txOutRef = inputs.head.outRef,
            datum = Some(vestingDatum.toData)
          )
        )
        // assert(result2.isSuccess, "Should fail")
    }

    def printAdaInInputs(
        txInfo: TxInfo,
        vestingDatum: VestingDatum,
        redeemer: VestingRedeemer
    ): Unit = {
        println("All outputs: " + txInfo.outputs)
        val beneficiaryOutputs = txInfo.outputs.filter(txOut =>
            txOut.address.credential match
                case Credential.PubKeyCredential(pkh) => pkh === vestingDatum.beneficiary
                case _                                => false
        )
        println("Beneficiary Outputs: " + beneficiaryOutputs)
        val adaInOutputs = beneficiaryOutputs
            .map(txOut => txOut.value.getLovelace)
            .foldLeft(BigInt(0))(_ + _)
        println("ADA in Outputs: " + adaInOutputs)

        println("All inputs: " + txInfo.inputs)
        val beneficiaryInputs = txInfo.inputs.filter(txInInfo =>
            txInInfo.resolved.address.credential match
                case Credential.PubKeyCredential(pkh) => pkh === vestingDatum.beneficiary
                case _                                => false
        )
        println("Beneficiary Inputs: " + beneficiaryInputs)
        val adaInInputs = beneficiaryInputs
            .map(txInInfo => txInInfo.resolved.value.getLovelace)
            .foldLeft(BigInt(0))(_ + _)
        println("Reddemer amount: " + redeemer.amount)
        println("ADA in Inputs: " + adaInInputs)
        println("Fee: " + txInfo.fee)
    }

    def printContractOutput(txInfo: TxInfo): Unit = {
        val ownInput = Utils.getOwnInput(txInfo.inputs, txInfo.inputs.head.outRef).resolved
        println("Own Input: " + ownInput)
        val contractAddress = ownInput.address
        println("Contract Address: " + contractAddress)

        println("TxInfo.outputs: " + txInfo.outputs)
        val contractOutputs = txInfo.outputs.filter(txOut => txOut.address === contractAddress)
        println("Contract Outputs: " + contractOutputs)

        val contractOutput = contractOutputs.head
        println("Contract Output: " + contractOutput)
        println("Contract Output Datum: " + contractOutput.datum)
    }

    def printAvailableAmout(txInfo: TxInfo, vestingDatum: VestingDatum): Unit = {
        def linearVesting(timestamp: BigInt): BigInt = {
            val min = vestingDatum.startTimestamp
            val max = vestingDatum.startTimestamp + vestingDatum.duration
            println("min: " + min + ", max: " + max)
            println("timestamp: " + timestamp)
            println("timestamp < min: " + (timestamp < min))
            println("timestamp >= max: " + (timestamp >= max))

            if timestamp < min then 0
            else if timestamp >= max then vestingDatum.amount
            else
                vestingDatum.amount * (timestamp - vestingDatum.startTimestamp) / vestingDatum.duration
                // 20_000_000L * (1609459200000 - 1609459200000) / 31536000000
        }
        val txEarliestTime = txInfo.validRange.from.boundType match
            case Finite(t) => t
            case _         => BigInt(0)
        val linearVestingTime = linearVesting(txEarliestTime)
        println("Linear Vesting Time: " + linearVestingTime)
        val ownInput = Utils.getOwnInput(txInfo.inputs, txInfo.inputs.head.outRef).resolved
        // println("Own Input: " + ownInput)
        val contractAmount = ownInput.value.getLovelace
        println("Contract Amount: " + contractAmount)
        val released = vestingDatum.amount - contractAmount // Can it be negative?
        println("Released Amount: " + released)
        val availableAmount = linearVestingTime - released
        println("Available Amount: " + availableAmount)
    }

}
// Fix first test crashing in Utils.scala getOwnInput => done
// Create test with 2 ScriptContexts
// Test fail => done
// change adaInInputs name
