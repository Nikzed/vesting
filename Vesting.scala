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
import scalus.builtin.FromData
import scalus.builtin.Data.toData
import scalus.ledger.api.v1.IntervalBoundType.*

case class VestingData(lockUntil: BigInt, ownerPKH: PubKeyHash, beneficiaryPKH: PubKeyHash) derives FromData, ToData 

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
        val vestingData: VestingData = recievedData.to[VestingData]

        val signedByOwner = tx.signatories.contains(vestingData.ownerPKH)
        val signedByBeneficiary = tx.signatories.contains(vestingData.beneficiaryPKH)

        val validAfter = tx.validRange.from.boundType match {
            case Finite(t) => t >= vestingData.lockUntil
            case NegInf    => false
            case PosInf    => false
        }

        require(
            signedByOwner || (signedByBeneficiary && validAfter),
            "Wrong case"
        )
    }
