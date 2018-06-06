package org.ergoplatform.explorer

import org.ergoplatform.modifiers.history.{BlockTransactions, Header}
import org.ergoplatform.modifiers.mempool.ErgoTransaction
import scorex.crypto.encode.Base16
import sigmastate.interpreter.SerializedProverResult

object InputsWriter {

  val table = "inputs"
  val fields = Seq("id", "tx_id", "output", "signature")
  val fieldsString = fields.mkString("(", ", ", ")")

  def dataString(h: Header, tx: ErgoTransaction): String = {
    val txId = Base16.encode(tx.id)
    val is = tx.inputs.map { in =>
      val id = Base16.encode(in.boxId)
      val proofBytes = SerializedProverResult.serializer.toBytes(in.spendingProof)
      val signature = Base16.encode(proofBytes)
      s"('$id', '$txId', '$id', '$signature')"
    }
    is.mkString(", ")
  }

  def dataStrings(h: Header, bt: BlockTransactions): String = {
    bt.transactions.map { tx =>
      dataString(h, tx)
    }.mkString(", ")
  }

}
