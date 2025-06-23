package validator

import scalus.prelude.List
import scalus.ledger.api.v3.TxInInfo
import scalus.ledger.api.v3.TxOutRef

object Utils {
    def getOwnInput(inputs: List[TxInInfo], ownRef: TxOutRef): TxInInfo =
        inputs
            .find(_.outRef == ownRef)
            .getOrElse(
              throw new NoSuchElementException(
                s"Input with reference $ownRef not found in transaction inputs."
              )
            )
}
