package validator

import com.bloxbean.cardano.client.account.Account
import com.bloxbean.cardano.client.api.model.{Amount, Result, Utxo}
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
import com.bloxbean.cardano.client.plutus.spec.PlutusV3Script
import scalus.builtin.Data.*
import scalus.builtin.ToData.*
import scalus.builtin.ToData.toData
import scalus.ledger.api.v1.PubKeyHash

import java.util.Optional
import scala.util.control.Breaks.*

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

    private val script = PlutusV3Script
        .builder()
        .cborHex(VestingScript.doubleCborHex)
        .build()

    private val scriptAddressBech32 =
        AddressProvider.getEntAddress(script, network).toBech32()

    def lock(amountAda: Long, datum: Data): Unit = {
        val tx = new Tx()
            .from(sender.getBaseAddress.getAddress)
            .payToContract(scriptAddressBech32, Amount.ada(amountAda), toPlutusData(datum))

        val signedTx = QuickTxBuilder(backendService)
            .compose(tx)
            .withSigner(SignerProviders.signerFrom(sender))
            .buildAndSign()

        println("Submitting Lock TX...")
        val result: Result[String] =
            backendService.getTransactionService.submitTransaction(signedTx.serialize)
        println(s"Result is: ${result}")

        if (result.isSuccessful()) {
            println(s"Lock Tx submitted successfully: ${result.getValue}")
            println("Waiting for UTxO to appear at script address...")
        } else {
            println(s"Lock Tx submission failed: ${result.getResponse}")
            sys.exit(1)
        }
    }

    def waitForUtxo(datum: Data): Utxo = {
        var scriptUtxo: Optional[Utxo] = Optional.empty()
        var attempts = 0
        val maxAttempts = 20 // 5 minutes timeout
        breakable {
            while (attempts < maxAttempts) {
                println(s"Searching for UTXO... (Attempt ${attempts + 1}/$maxAttempts)")
                scriptUtxo = ScriptUtxoFinders.findFirstByDatumHashUsingDatum(
                  utxoSupplier,
                  scriptAddressBech32,
                  toPlutusData(datum)
                )
                if (scriptUtxo.isPresent) {
                    println("Found UTXO!")
                    break
                }
                attempts += 1
                Thread.sleep(15 * 1000) // Wait 15 seconds
            }
        }
        scriptUtxo.orElseThrow(() =>
            new NoSuchElementException(
              s"UTXO with datum not found at $scriptAddressBech32 after $maxAttempts attempts."
            )
        )
    }

    def unlock(datum: Data, redeemer: Data, beneficiaryPubKeyHash: ByteString): Unit = {
        val scriptUtxo = waitForUtxo(datum)

        val claimAmount = scriptUtxo.getAmount
            .stream()
            .filter(_.getUnit == LOVELACE)
            .findFirst()
            .orElseThrow()

        val protocolParams = backendService.getEpochService.getProtocolParameters.getValue
        val currentSlot = backendService.getBlockService.getLatestBlock.getValue.getSlot

        val vestingRedeemer = redeemer.to[VestingRedeemer]
        val remainingAmount = claimAmount.getQuantity.subtract(vestingRedeemer.amount.bigInteger)

        val scriptTx = if (remainingAmount.compareTo(java.math.BigInteger.ZERO) > 0) {
            println("Building partial withdrawal transaction...")
            ScriptTx()
                .collectFrom(scriptUtxo, toPlutusData(redeemer))
                .payToAddress(
                  sender.baseAddress(),
                  Amount.lovelace(vestingRedeemer.amount.bigInteger)
                )
                .payToContract(
                  scriptAddressBech32,
                  Amount.lovelace(remainingAmount),
                  toPlutusData(datum)
                )
                .attachSpendingValidator(script)
        } else {
            println("Building full withdrawal transaction...")
            ScriptTx()
                .collectFrom(scriptUtxo, toPlutusData(redeemer))
                .payToAddress(sender.baseAddress(), claimAmount)
                .attachSpendingValidator(script)
        }

        val signedTx = QuickTxBuilder(backendService)
            .compose(scriptTx)
            .feePayer(sender.baseAddress())
            .withSigner(SignerProviders.signerFrom(sender))
            .validFrom(currentSlot)
            .withRequiredSigners(beneficiaryPubKeyHash.bytes)
            .ignoreScriptCostEvaluationError(false)
            .withTxEvaluator(ScalusTransactionEvaluator(protocolParams, utxoSupplier))
            .buildAndSign()

        println("Submitting Spend TX...")
        val result = backendService.getTransactionService.submitTransaction(signedTx.serialize)
        println(s"Result is: ${result}")
    }

    def main(args: Array[String]): Unit = {
        val beneficiary = ByteString.fromArray(sender.hdKeyPair().getPublicKey.getKeyHash)
        val beneficiaryPKH = PubKeyHash(beneficiary)

        val lockAmoutADA = 12L
        val lockAmoutLovelace = lockAmoutADA * 1_000_000L

        val latestBlock = backendService.getBlockService.getLatestBlock.getValue
        val currentTime = latestBlock.getTime * 1000L

        val startTime = currentTime - (3600L * 1000) // Vesting started 1 hour ago
        val duration = 1800L * 1000 // Vesting lasted 30 minutes

        val datum = VestingDatum(beneficiaryPKH, startTime, duration, lockAmoutLovelace)

        val redeemer = VestingRedeemer(lockAmoutLovelace)

        println(s"Script Address: $scriptAddressBech32")
        println(s"Beneficiary PKH: ${beneficiary.toHex}")
        println(s"Locking ${lockAmoutADA} ADA...")
        println(s"Vesting started at: ${startTime} (${java.util.Date(startTime)})")
        println(
          s"Vesting ends at: ${startTime + duration} (${java.util.Date(startTime + duration)})"
        )
        println(
          s"Current time from blockchain: ${currentTime} (${java.util.Date(currentTime)})"
        )

        lock(lockAmoutADA, datum.toData)
        unlock(datum.toData, redeemer.toData, beneficiary)
    }
