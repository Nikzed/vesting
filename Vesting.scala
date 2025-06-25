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

case class VestingDatum(
    beneficiary: PubKeyHash,
    startTimestamp: PosixTime,
    duration: PosixTime,
    amount: Lovelace
) derives FromData,
      ToData

@Compile
object VestingDatum

case class VestingRedeemer(amount: Lovelace) derives FromData, ToData

@Compile
object VestingRedeemer

@Compile
object Vesting extends Validator:
    override def spend(
        datum: Option[Data],
        redeemer: Data,
        tx: TxInfo,
        ownRef: TxOutRef
    ): Unit = {
        val Some(recievedData) = datum: @unchecked
        val vestingDatum: VestingDatum = recievedData.to[VestingDatum]
        val VestingRedeemer(declaredAmount) = redeemer.to[VestingRedeemer]

        val ownInput: TxInInfo = Utils.getOwnInput(tx.inputs, ownRef)
        val contractAddress = ownInput.resolved.address // check
        val contractAmount = ownInput.resolved.value
            .get(ByteString.empty) // Will be always 0 I guess
            .flatMap(_.get(ByteString.empty))
            .getOrElse(BigInt(0))

        val contractOutputs = tx.outputs.filter(_.address === contractAddress)

        val beneficiaryInputs = tx.inputs.filter(_.resolved.address.credential match
            case Credential.PubKeyCredential(pkh) => pkh === vestingDatum.beneficiary
            case _                                => false
        )

        val beneficiaryOutputs = tx.outputs.filter(_.address.credential match
            case Credential.PubKeyCredential(pkh) => pkh === vestingDatum.beneficiary
            case _                                => false
        )

        val txEarliestTime = tx.validRange.from.boundType match
            case Finite(t) => t
            case _         => BigInt(0)

        val released = vestingDatum.amount - contractAmount

        def linearVesting(total: BigInt, timestamp: BigInt): BigInt = {
            val min = vestingDatum.startTimestamp
            val max = vestingDatum.startTimestamp + vestingDatum.duration
            if timestamp < min then 0
            else if timestamp > max then total
            else total * (timestamp - vestingDatum.startTimestamp) / vestingDatum.duration
        }

        val releaseAmount = linearVesting(vestingDatum.amount, txEarliestTime) - released

        require(
          tx.signatories.contains(vestingDatum.beneficiary),
          "No signature from beneficiary"
        )
        require(
          declaredAmount == releaseAmount,
          s"Declared amount $declaredAmount does not match calculated amount $releaseAmount"
        )

        val adaInInputs = beneficiaryInputs
            .map(
              _.resolved.value
                  .get(ByteString.empty)
                  .flatMap(_.get(ByteString.empty))
                  .getOrElse(BigInt(0))
            )
            .foldLeft(BigInt(0))(_ + _) // foldLeft should add all values

        val adaInOutputs = beneficiaryOutputs
            .map(
              _.value
                  .get(ByteString.empty)
                  .flatMap(_.get(ByteString.empty))
                  .getOrElse(BigInt(0))
            )
            .foldLeft(BigInt(0))(_ + _)

        val expectedOutput = declaredAmount + adaInInputs - tx.fee

        require(
          adaInOutputs == expectedOutput,
          s"Beneficiary output mismatch: got $adaInOutputs, expected $expectedOutput"
        )

        if declaredAmount == contractAmount then ()
        else require(contractOutputs.length == BigInt(1), "Expected exactly one contract output")

        val contractOutput = contractOutputs.head
        contractOutput.datum match
            case OutputDatum.OutputDatum(inlineData) =>
                val contractDatum = inlineData.to[VestingDatum]
                require(
                  contractDatum.toData == vestingDatum.toData,
                  "VestingDatum mismatch"
                )
            case _ => require(false, "Expected inline datum")
    }
