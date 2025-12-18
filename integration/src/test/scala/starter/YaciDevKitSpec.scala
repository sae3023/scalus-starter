package starter

import com.bloxbean.cardano.client.account.Account
import com.bloxbean.cardano.client.api.UtxoSupplier
import com.bloxbean.cardano.client.api.model.{ProtocolParams, Utxo}
import com.bloxbean.cardano.client.backend.api.{BackendService, TransactionService}
import com.bloxbean.cardano.client.common.model.Network
import com.bloxbean.cardano.yaci.test.YaciCardanoContainer
import org.scalatest.{BeforeAndAfterAll, Suite}
import scalus.cardano.address.Network as ScalusNetwork
import scalus.cardano.ledger.{CardanoInfo, SlotConfig, ProtocolParams as ScalusProtocolParams, *}
import scalus.cardano.node.BlockfrostProvider
import scalus.cardano.txbuilder.TransactionSigner
import scalus.cardano.wallet.BloxbeanAccount
import scalus.utils.await
import sttp.client4.DefaultFutureBackend
import com.bloxbean.cardano.client.api.model.{ProtocolParams => BloxbeanProtocolParams}

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration.*
import scala.jdk.CollectionConverters.*

// Provide sttp backend for BlockfrostProvider
given sttp.client4.Backend[scala.concurrent.Future] = DefaultFutureBackend()

/** Configuration for Yaci DevKit container */
case class YaciDevKitConfig(
    enableLogs: Boolean = false,
    containerName: String = "scalus-starter-yaci-devkit"
)

/** Wrapper for Yaci DevKit container with helper methods */
class YaciDevKit(
    val container: YaciCardanoContainer,
    val account: Account,
    val config: YaciDevKitConfig
) {

    /** Get backend service for transaction operations */
    def getBackendService: BackendService = container.getBackendService

    /** Get UTXO supplier for querying available UTXOs */
    def getUtxoSupplier: UtxoSupplier = container.getUtxoSupplier

    /** Get protocol parameters */
    def getProtocolParams: ProtocolParams =
        container.getBackendService.getEpochService.getProtocolParameters().getValue

    /** Get transaction service for submitting transactions */
    def getTransactionService: TransactionService =
        container.getBackendService.getTransactionService

    /** Get UTXOs for an address */
    def getUtxos(address: String): List[Utxo] =
        getUtxoSupplier.getAll(address).asScala.toList

    /** Get total lovelace balance for an address */
    def getLovelaceBalance(address: String): BigInt = {
        val utxos = getUtxos(address)
        utxos.map(u => BigInt(u.getAmount.asScala.head.getQuantity)).sum
    }

    /** Wait for transaction to be confirmed */
    def waitForTransaction(
        txHash: String,
        maxAttempts: Int = 30,
        delayMs: Long = 1000
    ): Boolean = {
        def checkTx(attempts: Int): Boolean = {
            if (attempts >= maxAttempts) {
                false
            } else {
                try {
                    val tx = getTransactionService.getTransaction(txHash)
                    tx.isSuccessful && tx.getResponse != null
                } catch {
                    case _: Exception =>
                        Thread.sleep(delayMs)
                        checkTx(attempts + 1)
                }
            }
        }
        checkTx(0)
    }

    /** Submit transaction and wait for confirmation */
    def submitAndWait(txBytes: Array[Byte]): Either[String, String] = {
        try {
            val result = getTransactionService.submitTransaction(txBytes)
            if (result.isSuccessful) {
                val txHash = result.getValue
                if (waitForTransaction(txHash)) {
                    Right(txHash)
                } else {
                    Left(s"Transaction $txHash did not confirm in time")
                }
            } else {
                Left(s"Transaction submission failed: ${result.getResponse}")
            }
        } catch {
            case e: Exception =>
                Left(s"Transaction submission error: ${e.getMessage}")
        }
    }

    /** Stop the container */
    def stop(): Unit = container.stop()
}

/** Singleton container holder for sharing across test suites */
object YaciDevKitContainer {
    private var _container: YaciCardanoContainer = _
    private var _refCount: Int = 0
    private val lock = new Object()

    def acquire(config: YaciDevKitConfig): YaciCardanoContainer = lock.synchronized {
        if (_container == null) {
            _container = createContainer(config)
            _container.start()
        }
        _refCount += 1
        _container
    }

    def release(): Unit = lock.synchronized {
        _refCount -= 1
        // Don't stop the container - let testcontainers/ryuk handle cleanup
        // This allows reuse across test runs when reuse is enabled
    }

    private def createContainer(config: YaciDevKitConfig): YaciCardanoContainer = {
        val container = new YaciCardanoContainer()
        container.withCreateContainerCmdModifier(cmd => cmd.withName(config.containerName))
        container.withReuse(true)

        if (config.enableLogs) {
            container.withLogConsumer(frame => println(s"[Yaci] ${frame.getUtf8String}"))
        }

        container
    }
}

/** Base trait for integration tests using Yaci DevKit with ScalaTest
  *
  * Usage:
  * {{{
  * class MyIntegrationTest extends AnyFunSuite with YaciDevKitSpec {
  *   test("submit transaction to devnet") {
  *     val appCtx = createAppCtx("TokenName")
  *     // Use appCtx for testing
  *   }
  * }
  * }}}
  */
trait YaciDevKitSpec extends BeforeAndAfterAll { self: Suite =>

    /** Override this to customize the Yaci DevKit configuration */
    def yaciDevKitConfig: YaciDevKitConfig = YaciDevKitConfig()

    /** Fixed mnemonic for reproducible tests (same as Yaci CLI default) */
    val testMnemonic: String =
        "test test test test test test test test test test test test test test test test test test test test test test test sauce"

    /** Network configuration matching Yaci DevKit */
    val yaciNetwork: Network = new Network(0, 42)

    /** Test account created from the fixed mnemonic */
    lazy val testAccount: Account = Account.createFromMnemonic(yaciNetwork, testMnemonic)

    /** Standard derivation path for Cardano payment keys */
    private val PaymentDerivationPath = "m/1852'/1815'/0'/0/0"

    private var _container: YaciCardanoContainer = _
    private var _yaciDevKit: YaciDevKit = _

    /** Get the YaciDevKit wrapper with helper methods */
    def yaciDevKit: YaciDevKit = _yaciDevKit

    /** Get the running container */
    def container: YaciCardanoContainer = _container

    override def beforeAll(): Unit = {
        super.beforeAll()
        _container = YaciDevKitContainer.acquire(yaciDevKitConfig)
        _yaciDevKit = new YaciDevKit(_container, testAccount, yaciDevKitConfig)
    }

    override def afterAll(): Unit = {
        YaciDevKitContainer.release()
        super.afterAll()
    }

    /** Convert BloxBean ProtocolParams to Scalus ProtocolParams */
    private def convertProtocolParams(bp: BloxbeanProtocolParams): ScalusProtocolParams = {
        // Extract cost models - BloxBean returns Map<String, Map<String, Long>>
        val costModels = {
            val models = scala.collection.mutable.Map[Int, IndexedSeq[Long]]()
            if (bp.getCostModels != null) {
                val cm = bp.getCostModels
                if (cm.get("PlutusV1") != null) {
                    models(Language.PlutusV1.languageId) = cm.get("PlutusV1").asScala.values.map(_.longValue()).toIndexedSeq
                }
                if (cm.get("PlutusV2") != null) {
                    models(Language.PlutusV2.languageId) = cm.get("PlutusV2").asScala.values.map(_.longValue()).toIndexedSeq
                }
                if (cm.get("PlutusV3") != null) {
                    models(Language.PlutusV3.languageId) = cm.get("PlutusV3").asScala.values.map(_.longValue()).toIndexedSeq
                }
            }
            CostModels(models.toMap)
        }

        ScalusProtocolParams(
          collateralPercentage = bp.getCollateralPercent.longValue(),
          committeeMaxTermLength = Option(bp.getCommitteeMaxTermLength).map(_.longValue()).getOrElse(0L),
          committeeMinSize = Option(bp.getCommitteeMinSize).map(_.longValue()).getOrElse(0L),
          costModels = costModels,
          dRepActivity = Option(bp.getDrepActivity).map(_.longValue()).getOrElse(0L),
          dRepDeposit = Option(bp.getDrepDeposit).map(_.longValue()).getOrElse(0L),
          dRepVotingThresholds = DRepVotingThresholds(
            motionNoConfidence = UnitInterval.zero,
            committeeNormal = UnitInterval.zero,
            committeeNoConfidence = UnitInterval.zero,
            updateToConstitution = UnitInterval.zero,
            hardForkInitiation = UnitInterval.zero,
            ppNetworkGroup = UnitInterval.zero,
            ppEconomicGroup = UnitInterval.zero,
            ppTechnicalGroup = UnitInterval.zero,
            ppGovGroup = UnitInterval.zero,
            treasuryWithdrawal = UnitInterval.zero
          ),
          executionUnitPrices = ExUnitPrices(
            priceMemory = NonNegativeInterval(bp.getPriceMem.doubleValue()),
            priceSteps = NonNegativeInterval(bp.getPriceStep.doubleValue())
          ),
          govActionDeposit = Option(bp.getGovActionDeposit).map(_.longValue()).getOrElse(0L),
          govActionLifetime = Option(bp.getGovActionLifetime).map(_.longValue()).getOrElse(0L),
          maxBlockBodySize = bp.getMaxBlockSize.toLong,
          maxBlockExecutionUnits = ExUnits(
            memory = bp.getMaxBlockExMem.toLong,
            steps = bp.getMaxBlockExSteps.toLong
          ),
          maxBlockHeaderSize = bp.getMaxBlockHeaderSize.toLong,
          maxCollateralInputs = bp.getMaxCollateralInputs.toLong,
          maxTxExecutionUnits = ExUnits(
            memory = bp.getMaxTxExMem.toLong,
            steps = bp.getMaxTxExSteps.toLong
          ),
          maxTxSize = bp.getMaxTxSize.toLong,
          maxValueSize = bp.getMaxValSize.toLong,
          minFeeRefScriptCostPerByte = Option(bp.getMinFeeRefScriptCostPerByte).map(_.longValue()).getOrElse(15L),
          minPoolCost = bp.getMinPoolCost.toLong,
          monetaryExpansion = bp.getRho.doubleValue(),
          poolPledgeInfluence = bp.getA0.doubleValue(),
          poolRetireMaxEpoch = bp.getEMax.toLong,
          poolVotingThresholds = PoolVotingThresholds(
            motionNoConfidence = UnitInterval.zero,
            committeeNormal = UnitInterval.zero,
            committeeNoConfidence = UnitInterval.zero,
            hardForkInitiation = UnitInterval.zero,
            ppSecurityGroup = UnitInterval.zero
          ),
          protocolVersion = ProtocolVersion(
            major = bp.getProtocolMajorVer,
            minor = bp.getProtocolMinorVer
          ),
          stakeAddressDeposit = bp.getKeyDeposit.toLong,
          stakePoolDeposit = bp.getPoolDeposit.toLong,
          stakePoolTargetNum = bp.getNOpt.toLong,
          treasuryCut = bp.getTau.doubleValue(),
          txFeeFixed = bp.getMinFeeB.toLong,
          txFeePerByte = bp.getMinFeeA.toLong,
          utxoCostPerByte = bp.getCoinsPerUtxoSize.toLong
        )
    }

    /** Create AppCtx from the running YaciCardanoContainer */
    def createAppCtx(tokenName: String): AppCtx = {
        val account = Account.createFromMnemonic(yaciNetwork, testMnemonic)
        // Empty API key for local Yaci Store (Blockfrost-compatible API)
        // Strip trailing slash to avoid double-slash in URLs
        val baseUrl = _container.getYaciStoreApiUrl.stripSuffix("/")
        val provider = BlockfrostProvider("", baseUrl)

        // Get protocol parameters from container's BackendService (more reliable than HTTP API)
        val bloxbeanParams = _container.getBackendService.getEpochService.getProtocolParameters().getValue
        val protocolParams = convertProtocolParams(bloxbeanParams)

        // Yaci DevKit uses slot length of 1 second and start time of 0
        val yaciSlotConfig = SlotConfig(
          zeroTime = 0L,
          zeroSlot = 0L,
          slotLength = 1000
        )

        val cardanoInfo = CardanoInfo(protocolParams, ScalusNetwork.Testnet, yaciSlotConfig)

        // Use BloxbeanAccount for proper HD key signing
        val bloxbeanAccount = BloxbeanAccount(ScalusNetwork.Testnet, testMnemonic, PaymentDerivationPath)
        val signer = new TransactionSigner(Set(bloxbeanAccount.paymentKeyPair))

        new AppCtx(
          cardanoInfo,
          provider,
          account,
          signer,
          tokenName
        )
    }
}
