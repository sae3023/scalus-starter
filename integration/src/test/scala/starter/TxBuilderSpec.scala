package starter

import org.scalatest.BeforeAndAfterAll
import org.scalatest.funsuite.AnyFunSuite
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks
import scalus.builtin.ByteString
import scalus.cardano.ledger.AssetName
import scalus.utils.await

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration.*
import scala.util.Try

class TxBuilderSpec extends AnyFunSuite with ScalaCheckPropertyChecks with BeforeAndAfterAll {

    private val appCtx = AppCtx.yaciDevKit("CO2 Tonne")
    private val txBuilder = TxBuilder(appCtx)

    override def beforeAll(): Unit = {
        // Check if Yaci DevKit is available by trying to query UTXOs
        val available = Try {
            appCtx.provider
                .findUtxos(appCtx.address, None, None, None, None)
                .await(10.seconds)
        }
        if (available.isFailure) {
            cancel(
              "This test requires a Blockfrost API available. Start Yaci Devkit before running this test."
            )
        }
    }

    test("create minting transaction") {
        txBuilder.makeMintingTx(1000) match
            case Right(tx) =>
                val mint = tx.body.value.mint
                assert(mint.nonEmpty)
                val expectedAssetName = AssetName(appCtx.tokenNameByteString)
                val mintedAmount = mint
                    .flatMap(_.assets.get(appCtx.mintingScript.policyId))
                    .flatMap(_.get(expectedAssetName))
                assert(mintedAmount.contains(1000L))
                // Check that the transaction has witnesses
                assert(tx.witnessSet.vkeyWitnesses.toSet.nonEmpty)
            case Left(err) => fail(err)
    }

}
