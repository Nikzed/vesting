package validator

import com.bloxbean.cardano.client.account.Account
import com.bloxbean.cardano.client.api.model.Amount
import com.bloxbean.cardano.client.backend.api.DefaultProtocolParamsSupplier
import com.bloxbean.cardano.client.backend.api.DefaultUtxoSupplier
import com.bloxbean.cardano.client.backend.blockfrost.common.Constants
import com.bloxbean.cardano.client.backend.blockfrost.service.BFBackendService
import com.bloxbean.cardano.client.function.helper.*
import com.bloxbean.cardano.client.quicktx.{QuickTxBuilder, ScriptTx, Tx}
import com.bloxbean.cardano.client.address.AddressProvider
import com.bloxbean.cardano.client.common.model.Networks
import com.bloxbean.cardano.client.common.CardanoConstants.LOVELACE
import scalus.*
import scalus.bloxbean.Interop.toPlutusData
import scalus.bloxbean.ScalusTransactionEvaluator
import scalus.builtin.{ByteString, Data}
import scalus.utils.Utils
import com.bloxbean.cardano.client.plutus.spec.PlutusV2Script
import scalus.builtin.Data.*
import scalus.builtin.ToData.*
import scalus.builtin.ToData.toData
import scalus.bloxbean.Interop
import scalus.bloxbean.Interop.getAddress
import scalus.ledger.api.v1.Credential

object VestingOffChain:

    private val mnemonic = sys.env("MNEMONIC")
    private val blockfrostApiKey = sys.env("BLOCKFROST_API_KEY")

    private val network = Networks.preprod()
    private val sender = new Account(network, mnemonic)

    private val backendService =
        new BFBackendService(Constants.BLOCKFROST_PREPROD_URL, blockfrostApiKey)

    private val utxoSupplier = new DefaultUtxoSupplier(backendService.getUtxoService)
    private val protocolParamsSupplier = new DefaultProtocolParamsSupplier(
      backendService.getEpochService
    )

    private val script = PlutusV2Script
        .builder()
        .cborHex(VestingScript.doubleCborHex)
        .build()

    private val scriptAddressBech32 =
        AddressProvider.getEntAddress(script, network).toBech32()

    def lock(amountAda: Long, datum: Data): Unit =
        val tx = new Tx()
            .from(sender.getBaseAddress.getAddress)
            .payToContract(scriptAddressBech32, Amount.ada(amountAda), toPlutusData(datum))

        val signedTx = QuickTxBuilder(backendService)
            .compose(tx)
            .withSigner(SignerProviders.signerFrom(sender))
            .buildAndSign()

        println("Submitting Lock TX...")
        val result = backendService.getTransactionService.submitTransaction(signedTx.serialize)
        println(s"Result is: ${result}")

    def unlock(datum: Data, redeemer: Data, beneficiaryPubKeyHash: ByteString): Unit =
        val scriptUtxo = ScriptUtxoFinders
            .findFirstByDatumHashUsingDatum(utxoSupplier, scriptAddressBech32, toPlutusData(datum))
            .orElseThrow()

        val claimAmount = scriptUtxo.getAmount
            .stream()
            .filter(_.getUnit == LOVELACE)
            .findFirst()
            .orElseThrow()

        val protocolParams = backendService.getEpochService.getProtocolParameters().getValue

        val scriptTx = new ScriptTx()
            .collectFrom(scriptUtxo, toPlutusData(redeemer))
            .payToAddress(sender.baseAddress(), claimAmount)
            .attachSpendingValidator(script)

        val signedTx = QuickTxBuilder(backendService)
            .compose(scriptTx)
            .feePayer(sender.baseAddress())
            .withSigner(SignerProviders.signerFrom(sender))
            .withRequiredSigners(beneficiaryPubKeyHash.bytes)
            .ignoreScriptCostEvaluationError(false)
            .withTxEvaluator(ScalusTransactionEvaluator(protocolParams, utxoSupplier))
            .buildAndSign()

        println("Submitting Spend TX...")
        val result = backendService.getTransactionService.submitTransaction(signedTx.serialize)
        println(s"Result is: ${result}")

    def main(args: Array[String]): Unit =
        val beneficiary = ByteString.fromArray(sender.hdKeyPair().getPublicKey.getKeyHash)
        // val beneficiary = sender.getBaseAddress().getPaymentCredentialHash()
        val beneficiaryPKH = Interop.getAddress(sender.getBaseAddress).credential match
            case Credential.PubKeyCredential(hash) => hash
            case _ => throw new RuntimeException("Failed to get beneficiary address")

        val datum = VestingDatum(beneficiaryPKH, 1753368834L, 1753368834L + 100L, 15)
        val redeemer = VestingRedeemer(15)

        lock(15, datum.toData)
        Thread.sleep(60 * 1000)
        unlock(datum.toData, redeemer.toData, beneficiary)
