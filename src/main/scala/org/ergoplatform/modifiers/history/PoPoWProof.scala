package org.ergoplatform.modifiers.history

import com.google.common.primitives.{Bytes, Shorts}
import org.ergoplatform.mining.PowScheme
import org.ergoplatform.modifiers.ErgoPersistentModifier
import org.ergoplatform.settings.{Algos, Constants}
import scorex.core._
import scorex.core.serialization.Serializer
import scorex.core.validation.ModifierValidator
import scorex.core.utils.ScorexEncoding

import scala.annotation.tailrec
import scala.math.BigInt
import scala.util.Try

case class PoPoWProof(m: Byte,
                      k: Byte,
                      i: Byte,
                      innerchain: Seq[Header],
                      suffix: Seq[Header],
                      override val sizeOpt: Option[Int] = None)(implicit powScheme: PowScheme) extends Comparable[PoPoWProof] with Ordered[PoPoWProof]
  with ErgoPersistentModifier {

  override val modifierTypeId: ModifierTypeId = PoPoWProof.modifierTypeId

  override def parentId: ModifierId = ???

  override def serializedId: Array[Byte] = Algos.hash(bytes)

  override lazy val id: ModifierId = bytesToId(serializedId)

  override type M = PoPoWProof

  override lazy val serializer: Serializer[PoPoWProof] = new PoPoWProofSerializer(powScheme)
  // lca(C1, C2) = (C1 ∩ C2)[−1] π
  // needed at best-arg_m(πA, lca) ≥ best-arg_m(πB, lca)
  @tailrec
  final private def lca(
                         proofB: PoPoWProof,
                         proofABlockIdx: Int,
                         proofBBlockIdx: Int,
                         LCA: Option[Header] = None): Option[Header] = {

    if (proofABlockIdx >= 0 && proofBBlockIdx >= 0) {

      this.innerchain(proofABlockIdx) match {

        case a if a.extensionHash == proofB.innerchain(proofBBlockIdx).extensionHash =>
          Some(this.innerchain(proofABlockIdx))

        case a if a.height > proofB.innerchain(proofBBlockIdx).height =>
          lca(proofB, proofABlockIdx - 1, proofBBlockIdx)

        case _ => lca(proofB, proofABlockIdx, proofBBlockIdx - 1)
      }
    } else {
      None
    }
  }

  private def defineM(proof: PoPoWProof, startBlock: Option[Header]): Seq[Int] = {

    @tailrec // {b:}
    def generateInitialIdx(innerChain: Seq[Header], startBlock: Header, position: Int): Int =
      if (position >= 0)
        if (innerChain(position) == startBlock) position
        else generateInitialIdx(innerchain, startBlock, position - 1)
      else 0

    val initialIdx =
      if (startBlock.isEmpty) 0
      else generateInitialIdx(proof.innerchain, startBlock.get, proof.innerchain.length -1)

    def takeLevel(header: Header, level: Int = 0): Int = {
      if (Constants.MaxTarget / BigInt(1, header.serializedId) >= header.requiredDifficulty * BigInt(2).pow(level)) {
        level
      } else {
        takeLevel(header, level + 1)
      }
    }

    // {µ : |π ↑µ {b :}| ≥ m}
    proof.innerchain
      .map(block => Seq.fill(takeLevel(block))(1))
      .foldLeft(Seq.fill(proof.innerchain.length - 1)(0))((acc: Seq[Int], currnet: Seq[Int]) => {
        for{
          a <- acc
          b <- currnet
        } yield a + b
      })

  }

  private def bestArg(proof: PoPoWProof, lca: Option[Header]): BigInt = {
    val m = defineM(proof, lca)
    Seq.range(0, proof.innerchain.length - 1).foldLeft(0: BigInt)((a: BigInt,b: Int) =>{
      val check = BigInt(2).pow(b) * BigInt(m(b))
      if (check > a) check else a
    })
  }

  override def compare(proof: PoPoWProof): Int = {
    val lcaValue = lca(proof, this.innerchain.length - 1, proof.innerchain.length - 1)
    val proofA = bestArg(this, lcaValue)
    val proofB = bestArg(proof, lcaValue)
    proofA.compare(proofB)
  }

}

object PoPoWProof {
  val modifierTypeId: ModifierTypeId = ModifierTypeId @@ (105: Byte)
}

@SuppressWarnings(Array("TraversableHead", "CollectionIndexOnNonIndexedSeq"))
class PoPoWProofUtils(powScheme: PowScheme) extends ScorexEncoding with ModifierValidator{

  //todo: complete validation, no PoW validation, linking structure validation, genesis validation
  def validate(proof: PoPoWProof): Try[Unit] = {
    //todo: why initial difficulty here?
    val innerDifficulty: BigInt = Constants.InitialDifficulty * BigInt(2).pow(proof.i)
    failFast
      .validate(proof.suffix.lengthCompare(proof.k) == 0) {
        error(s"Incorrect suffix ${proof.suffix.length} != ${proof.k}")
      }
      .validate(proof.k >= 1) {
        error(s"k should positive, ${proof.k} given")
      }
      .validate(proof.m >= 1) {
        error(s"m should positive, ${proof.m} given")
      }
      .validate(proof.suffix.head.interlinks(proof.i) == proof.innerchain.last.id) {
        error(s"Incorrect link form suffix to innerchain in $proof")
      }
      .validate(proof.innerchain.length >= proof.m) {
        error(s"Innerchain length is not enough in $proof")
      }
      .validate(proof.innerchain.forall(h => powScheme.realDifficulty(h) >= innerDifficulty)) {
        error(s"Innerchain difficulty is not enough in $proof")
      }
      .validate(proof.suffix.sliding(2).filter(_.length == 2).forall(s => s(1).parentId == s.head.id)) {
        error(s"Suffix links are incorrect in $proof")
      }
      .validate(proof.innerchain.sliding(2).filter(_.length == 2).forall(s => s(1).interlinks(proof.i) == s.head.id)) {
        error(s"Innerchain links are incorrect in $proof")
      }
      .result
      .toTry
  }

  def isLevel(header: Header, level: Int): Boolean = {
    val headerDiff = powScheme.realDifficulty(header)
    val levelDiff = header.requiredDifficulty * BigInt(2).pow(level)
    headerDiff >= levelDiff
  }

  def maxLevel(header: Header): Int = {
    @tailrec
    def generateMaxLevel(level: Int): Int = {
      if (isLevel(header, level + 1)) {
        generateMaxLevel(level + 1)
      } else {
        level
      }
    }
    generateMaxLevel(0)
  }

  def constructInterlinkVector(parent: Header): Seq[ModifierId] = {
    if (parent.isGenesis) {
      //initialize interlink vector at first block after genesis
      Seq(parent.id)
    } else {

      val genesisId = parent.interlinks.head

      val tail = parent.interlinks.tail

      val pLevel = maxLevel(parent)

      val pLevels = if (pLevel > 0) Seq.fill(pLevel)(parent.id) else Seq.empty

      val priorLevels = if(tail.length > pLevel) tail.drop(pLevel) else Seq.empty

      (genesisId +: pLevels) ++ priorLevels
    }
  }
}

@SuppressWarnings(Array("TraversableHead"))
class PoPoWProofSerializer(powScheme: PowScheme) extends Serializer[PoPoWProof] {
  override def toBytes(obj: PoPoWProof): Array[Byte] = {
    val suffixTailBytes = scorex.core.utils.concatBytes(obj.suffix.tail.map { h =>
      val bytes = HeaderSerializer.bytesWithoutInterlinks(h)
      Bytes.concat(Shorts.toByteArray(bytes.length.toShort), bytes)
    })
    val innerchainBytes = scorex.core.utils.concatBytes(obj.innerchain.map { h =>
      val bytes = h.bytes
      Bytes.concat(Shorts.toByteArray(bytes.length.toShort), bytes)
    })
    Bytes.concat(Array(obj.m, obj.k, obj.i),
      Shorts.toByteArray(obj.suffix.head.bytes.length.toShort),
      obj.suffix.head.bytes,
      suffixTailBytes,
      Shorts.toByteArray(obj.innerchain.length.toShort),
      innerchainBytes)
  }

  @SuppressWarnings(Array("TryGet"))
  override def parseBytes(bytes: Array[Byte]): Try[PoPoWProof] = Try {
    val m = bytes.head
    val k = bytes(1)
    val i = bytes(2)
    val headSuffixLength = Shorts.fromByteArray(bytes.slice(3, 5))
    val headSuffix = HeaderSerializer.parseBytes(bytes.slice(5, 5 + headSuffixLength)).get

    def parseSuffixes(index: Int, acc: Seq[Header]): (Int, Seq[Header]) = {
      if (acc.lengthCompare(k.toInt) == 0)  {
        (index, acc.reverse)
      } else {
        val l = Shorts.fromByteArray(bytes.slice(index, index + 2))
        val headerWithoutInterlinks = HeaderSerializer.parseBytes(bytes.slice(index + 2, index + 2 + l)).get
        val interlinks = new PoPoWProofUtils(powScheme).constructInterlinkVector(acc.head)
        parseSuffixes(index + 2 + l, headerWithoutInterlinks.copy(interlinks = interlinks) +: acc)
      }
    }

    val (index, suffix) = parseSuffixes(5 + headSuffixLength, Seq(headSuffix))
    val innerchainLength = Shorts.fromByteArray(bytes.slice(index, index + 2))

    @tailrec
    def createInnerChain(index: Int, step: Int = 0, chain: Seq[Header] = Seq.empty): Seq[Header] = {
      if (step < innerchainLength) {
        val l = Shorts.fromByteArray(bytes.slice(index, index + 2))
        val header = HeaderSerializer.parseBytes(bytes.slice(index + 2, index + 2 + l)).get
        createInnerChain(index + 2 + l, step + 1, chain ++ Seq(header))
      } else {
        chain
      }
    }
    PoPoWProof(m, k, i, createInnerChain(index + 2), suffix, Some(bytes.length))(powScheme)
  }
}
