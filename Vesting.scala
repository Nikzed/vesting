package validator

import scalus.*
import scalus.builtin.*
import scalus.builtin.Data
import scalus.builtin.Builtins.trace
import scalus.ledger.api.v3.*
import scalus.prelude.*
import scalus.prelude.log
import scalus.prelude.Option.*
import scalus.prelude.Prelude.*
import scalus.prelude.given_Eq_ByteString

import scalus.builtin.FromData
import scalus.builtin.Data.toData
import scalus.ledger.api.v1.IntervalBoundType.*

// case class VestingData(lockUntil: BigInt, ownerPKH: PubKeyHash, beneficiaryPKH: PubKeyHash) derives FromData, ToData 

case class VestingDatum(beneficiary: PubKeyHash, startTimestamp: PosixTime, duration: PosixTime, amount: Lovelace) derives FromData, ToData

case class VestingRedeemer(amout: Lovelace) derives FromData, ToData

@Compile
object VestingData

@Compile
object Vesting extends Validator:
    // write a test for this from Aiken (Mesh) Vesting
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
        val contractAmount = ownInput.resolved.value
            .get(ByteString.empty) // Will be always 0 I guess
            .flatMap(_.get(ByteString.empty))
            .getOrElse(BigInt(0)) 

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
    }
