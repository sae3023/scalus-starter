package starter

import org.scalatest.BeforeAndAfterAll
import org.scalatest.funsuite.AnyFunSuite
import scalus.cardano.ledger.Transaction
import scalus.utils.await

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration.*
import scala.util.Try

/** This integration test mints and burns tokens.
  *
  * It requires a Blockfrost API available. Use Yaci Devkit to run this test.
  *
  * {{{
  *  devkit
  *  create-node
  *  start
  * }}}
  */
class MintingIT extends AnyFunSuite with BeforeAndAfterAll {

    private val appCtx = AppCtx.yaciDevKit("CO2 Tonne")
    private val txBuilder = TxBuilder(appCtx)

    private def submitTx(tx: Transaction): Either[String, String] =
        appCtx.provider.submit(tx).await(30.seconds).map(_.toHex).left.map(_.toString)

    private def waitBlock(): Unit = Thread.sleep(3000)

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

    test("mint and burn tokens") {
        val result = for
            // mint 1000 tokens
            tx <- txBuilder.makeMintingTx(1000)
            _ = println(s"minting tx: ${tx.id.toHex}")
            _ <- submitTx(tx)
            _ = waitBlock()
            // burn 1000 tokens
            burnedTx <- txBuilder.makeBurningTx(-1000)
            _ = println(s"burning tx: ${burnedTx.id.toHex}")
            _ <- submitTx(burnedTx)
            _ = waitBlock()
            // verify burn succeeded by checking we can query UTXOs
            utxos <- appCtx.provider
                .findUtxos(appCtx.address, None, None, None, None)
                .await(10.seconds)
                .left
                .map(_.toString)
        yield utxos
        result match
            case Right(utxos) =>
                // Check that no UTXOs contain the minted token
                val tokenUtxos = utxos.filter { case (_, utxo) =>
                    utxo.value.assets.assets.contains(appCtx.mintingScript.policyId)
                }
                assert(tokenUtxos.isEmpty, s"Expected no token UTXOs but found: $tokenUtxos")
            case Left(err) => fail(err)
    }
}
