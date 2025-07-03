package validator

import scalus.Compile
import scalus.prelude.List
import scalus.ledger.api.v3.TxInInfo
import scalus.ledger.api.v3.TxOutRef
import scalus.prelude.*

@Compile
object Utils {
    def getOwnInput(inputs: List[TxInInfo], ownRef: TxOutRef): TxInInfo = {
        inputs
            .find((input) => input.outRef === ownRef)
            .get
    }
}
