package org.ergoplatform

import org.ergoplatform.ErgoSanity.{PM, UTXO_ST}
import org.ergoplatform.modifiers.mempool.ErgoTransaction
import org.ergoplatform.nodeView.history.{ErgoHistory, ErgoSyncInfo}
import org.ergoplatform.nodeView.mempool.ErgoMemPool
import org.ergoplatform.utils.ErgoNodeViewHolderTestHelpers
import scorex.testkit.properties.NodeViewHolderTests

class ErgoSanityUTXO
  extends NodeViewHolderTests[ErgoTransaction, PM, UTXO_ST, ErgoSyncInfo, ErgoHistory, ErgoMemPool]
    with ErgoSanity[UTXO_ST]
    with ErgoNodeViewHolderTestHelpers {

  override def semanticallyInvalidModifier(state: UTXO_ST): PM = invalidErgoFullBlockGen.sample.get

}
