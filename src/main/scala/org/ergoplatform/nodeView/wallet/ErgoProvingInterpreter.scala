package org.ergoplatform.nodeView.wallet

import org.ergoplatform.{ErgoBox, ErgoLikeContext, ErgoLikeInterpreter, Input}
import org.ergoplatform.modifiers.mempool.{ErgoTransaction, UnsignedErgoTransaction}
import org.ergoplatform.nodeView.state.ErgoStateContext
import scapi.sigma.DLogProtocol.{DLogProverInput, ProveDlog}
import scapi.sigma.{DiffieHellmanTupleProverInput, SigmaProtocolPrivateInput}
import scorex.crypto.encode.Base58
import scorex.crypto.hash.Blake2b256
import sigmastate.AvlTreeData
import sigmastate.interpreter.{ContextExtension, ProverInterpreter}
import sigmastate.serialization.ValueSerializer
import sigmastate.utxo.CostTable

import scala.util.Try

object Address {

  def hash256(input: Array[Byte]) = Blake2b256(input)

  //todo: take Blake2b160 ?
  def hash160(input: Array[Byte]) = hash256(input).take(20)

  def p2pkh(pubkey: ProveDlog): String = {
    val bt = ValueSerializer.serialize(pubkey)
    val bth160 = hash160(bt)

    //add network identifier
    val withNetworkByte = (0: Byte) +: bth160

    val checksum = hash256(withNetworkByte).take(4)
    Base58.encode(withNetworkByte ++ checksum)
  }

}

class ErgoProvingInterpreter(seed: String, override val maxCost: Long = CostTable.ScriptLimit)
  extends ErgoLikeInterpreter(maxCost) with ProverInterpreter {

  def bytesToTrack(secret: DLogProverInput): Array[Byte] = bytesToTrack(secret.publicImage)

  def bytesToTrack(pubkey: ProveDlog): Array[Byte] = {
    ValueSerializer.serialize(pubkey)
  }


  override lazy val secrets: Seq[SigmaProtocolPrivateInput[_, _]] =
    dlogSecrets ++ dhSecrets

  lazy val dlogSecrets: Seq[DLogProverInput] = ErgoWallet.secretsFromSeed(seed).map(DLogProverInput.apply)

  lazy val dhSecrets: Seq[DiffieHellmanTupleProverInput] =
    (1 to 4).map(_ => DiffieHellmanTupleProverInput.random())

  def sign(unsignedTx: UnsignedErgoTransaction,
           boxesToSpend: IndexedSeq[ErgoBox],
           stateContext: ErgoStateContext): Option[ErgoTransaction] = Try {

    require(unsignedTx.inputs.length == boxesToSpend.length)
    val inputs = unsignedTx.inputs.zip(boxesToSpend).map { case (unsignedInput, inputBox) =>
      require(unsignedInput.boxId.sameElements(inputBox.id))

      val context =
        ErgoLikeContext(
          stateContext.height + 1,
          AvlTreeData(stateContext.digest, 32),
          boxesToSpend,
          unsignedTx,
          inputBox,
          ContextExtension.empty)

      val proverResult = prove(inputBox.proposition, context, unsignedTx.messageToSign).get
      Input(unsignedInput.boxId, proverResult)
    }

    ErgoTransaction(inputs, unsignedTx.outputCandidates)
  }.toOption //todo: handle errors
}