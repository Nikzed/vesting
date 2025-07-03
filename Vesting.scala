package validator

import scalus.*
import scalus.builtin.*
import scalus.ledger.api.v3.*
import scalus.prelude.*
import scalus.prelude.log
import scalus.prelude.Option.*
import scalus.prelude.given_Eq_ByteString
import scalus.ledger.api.v1.IntervalBoundType.*
import scalus.prelude.Eq
import scalus.ledger.api.v2.OutputDatum
import scalus.builtin.Data.{FromData, ToData, fromData, toData}
import scalus.ledger.api.v1.Value.getLovelace
import scalus.Compiler.compile

case class VestingDatum(
    beneficiary: PubKeyHash,
    startTimestamp: PosixTime,
    duration: PosixTime,
    amount: Lovelace // Maybe change to initialAmount
) derives FromData,
      ToData

@Compile
object VestingDatum

case class VestingRedeemer(amount: Lovelace) // Maybe change to amountToWithdraw
    derives FromData,
      ToData

@Compile
object VestingRedeemer

@Compile
object Vesting extends Validator:
    override def spend(
        datum: Option[Data],
        redeemer: Data,
        txInfo: TxInfo,
        txOutRef: TxOutRef
    ): Unit = {
        val Some(receivedData) = datum: @unchecked
        val vestingDatum: VestingDatum = receivedData.to[VestingDatum]
        val VestingRedeemer(requestedAmount) = redeemer.to[VestingRedeemer]

        val ownInput = Utils.getOwnInput(txInfo.inputs, txOutRef).resolved
        val contractAddress = ownInput.address
        val contractAmount = ownInput.value.getLovelace

        val contractOutputs =
            txInfo.outputs.filter(txOut => txOut.address === contractAddress) // I don't undertand

        val txEarliestTime = txInfo.validRange.from.boundType match
            case Finite(t) => t
            case _         => BigInt(0)

        val released = vestingDatum.amount - contractAmount

        def linearVesting(timestamp: BigInt): BigInt = {
            val min = vestingDatum.startTimestamp
            val max = vestingDatum.startTimestamp + vestingDatum.duration
            if timestamp < min then 0
            else if timestamp >= max then vestingDatum.amount // changed from >
            else
                vestingDatum.amount * (timestamp - vestingDatum.startTimestamp) / vestingDatum.duration
        } // test if you already released some amount

        val availableAmount = linearVesting(txEarliestTime) - released

        require(
          txInfo.signatories.contains(vestingDatum.beneficiary),
          "No signature from beneficiary"
        )
        require(
          requestedAmount <= availableAmount, // changed from ==
          "Declared amount does not match calculated amount"
        )

        val beneficiaryInputs = txInfo.inputs.filter(txInInfo =>
            txInInfo.resolved.address.credential match
                case Credential.PubKeyCredential(pkh) => pkh === vestingDatum.beneficiary
                case _                                => false
        ) // check
        val beneficiaryOutputs = txInfo.outputs.filter(txOut =>
            txOut.address.credential match
                case Credential.PubKeyCredential(pkh) => pkh === vestingDatum.beneficiary
                case _                                => false
        )

        val adaInInputs = beneficiaryInputs
            .map(txInInfo => txInInfo.resolved.value.getLovelace)
            .foldLeft(BigInt(0))(_ + _)
        val adaInOutputs = beneficiaryOutputs
            .map(txOut => txOut.value.getLovelace)
            .foldLeft(BigInt(0))(_ + _)

        val expectedOutput = requestedAmount + adaInInputs - txInfo.fee
        // Can be deleted, same as adaInOutputs == expectedOutput
        // require(
        //   adaInOutputs - adaInInputs == requestedAmount - tx.fee,
        //   "Beneficiary output mismatch"
        // )

        require(
          adaInOutputs == expectedOutput,
          "Beneficiary output mismatch"
        )

        if requestedAmount == contractAmount then ()
        else require(contractOutputs.length == BigInt(1), "Expected exactly one contract output")

        val contractOutput = contractOutputs.head
        contractOutput.datum match
            case OutputDatum.OutputDatum(inlineData) =>
                require(
                  inlineData == vestingDatum.toData, // This will always fail, inlineData is not VestingDatum
                  "VestingDatum mismatch"
                )
            case _ => fail("Expected inline datum")
    }

object VestingScript {
    val compiled = compile(Vesting.validate)
}
