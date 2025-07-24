package validator

import scalus.builtin.ByteString
import scalus.builtin.Builtins.blake2b_224
import scalus.builtin.Builtins.appendByteString
import scalus.ledger.api.v1.PubKeyHash
import scalus.ledger.api.v3.{TxId, TxOutRef}
import scalus.ledger.api.v1.ValidatorHash

object Mock {
    val rootHash: ByteString =
        ByteString.fromHex("a2c20c77887ace1cd986193e4e75babd8993cfd56995cd5cfce609c2")

    private def mockKeyHash(variation: BigInt): ByteString = {
        val variationBytes = ByteString.fromArray(variation.toByteArray)
        blake2b_224(appendByteString(variationBytes, rootHash))
    }

    def mockPubKeyHash(variation: BigInt): PubKeyHash = PubKeyHash(mockKeyHash(variation))

    def mockScriptHash(variation: BigInt): ValidatorHash =
        mockKeyHash(variation + 200)
}
