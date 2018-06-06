package org.ergoplatform.explorer

import org.ergoplatform.modifiers.history.{BlockTransactions, Header}
import org.ergoplatform.modifiers.mempool.ErgoTransaction
import scorex.crypto.encode.Base16

object OutputsWriter {

  val table = "outputs"
  val fields = Seq("id", "tx_id", "value", "spent", "script", "hash")
  val fieldsString = fields.mkString("(", ", ", ")")

  def dataString(h: Header, tx: ErgoTransaction): String = {
    val txId = Base16.encode(tx.id)
    val os = tx.outputCandidates.zipWithIndex.map { case (bx, idx) =>
      val ergoBox = bx.toBox(tx.id, idx.toShort)

      val id = Base16.encode(ergoBox.id)
      val script = Base16.encode(bx.proposition.bytes)
      val hash = script
      s"('$id', '$txId', ${bx.value}, FALSE, '$script', '$hash')"
    }
    os.mkString(", ")
  }

  def dataStrings(h: Header, bt: BlockTransactions): String = {
    bt.transactions.map { tx =>
      dataString(h, tx)
    }.mkString(", ")
  }

}
