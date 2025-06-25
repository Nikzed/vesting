package validator

import scalus.Compile
import scalus.prelude.List
import scalus.ledger.api.v3.TxInInfo
import scalus.ledger.api.v3.TxOutRef

@Compile
object Utils {
    def getOwnInput(inputs: List[TxInInfo], ownRef: TxOutRef): TxInInfo = {
        inputs
            .find(_.outRef.id.hash == ownRef.id.hash)
            .getOrElse(
              throw new NoSuchElementException( // I don't think it's ok to throw an exception here.
                "Reference not found in transaction inputs."
              )
            )
    }
}
