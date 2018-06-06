package org.ergoplatform.explorer

import scorex.core.ModifierId
import scorex.crypto.encode.Base16

object InterlinksWriter {

  val table = "interlinks"
  val fields = Seq("modifier_id", "block_id")
  val fieldsString = fields.mkString("(", ", ", ")")

  def dataString(headerId: ModifierId, modifierId: ModifierId): String = {
    val hId = Base16.encode(headerId)
    val mId = Base16.encode(modifierId)

    s"('$mId', '$hId')"
  }

  def dataStrings(list: Seq[(ModifierId, ModifierId)]): String = {
    list.map{ case (hId, mId) => dataString(hId, mId)} .mkString(", ")
  }

}
