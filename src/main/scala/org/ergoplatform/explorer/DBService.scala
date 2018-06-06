package org.ergoplatform.explorer

import doobie._
import doobie.implicits._
import doobie.postgres.implicits._
import cats.data._
import cats.implicits._
import cats.effect.IO
import doobie.free.connection.ConnectionIO
import doobie.util.fragment.Fragment
import doobie.util.transactor.Transactor
import org.ergoplatform.modifiers.ErgoFullBlock
import org.ergoplatform.modifiers.history.{ADProofs, BlockTransactions, Header}

object DBService {


  def insert(table: String, fieldsString: String, dataString: String): ConnectionIO[Int] = {
    if (dataString.length > 2) {
      Fragment.const(s"INSERT INTO $table $fieldsString VALUES $dataString;").update.run
    } else {
      doobie.free.connection.unit.map(_ => 0)
    }
  }


  def insertHeader(h: Header, adProofs: Option[ADProofs], bt: BlockTransactions): ConnectionIO[Int] = {
    import HeaderWriter._

    insert(table, fieldsString, dataString(h, adProofs, bt))
  }

  def insertInterlinks(h: Header): ConnectionIO[Int] = {
    import InterlinksWriter._

    val links = h.interlinks.map { l => (h.id, l) }.distinct
    insert(table, fieldsString, dataStrings(links))
  }

  def insertBlockTransactions(h: Header, bt: BlockTransactions): ConnectionIO[Int] = {
    import TransactionsWriter._

    insert(table, fieldsString, dataStrings(h, bt))
  }

  def insertInputs(h: Header, bt: BlockTransactions): ConnectionIO[Int] = {
    import InputsWriter._

    insert(table, fieldsString, dataStrings(h, bt))
  }

  def insertOutputs(h: Header, bt: BlockTransactions): ConnectionIO[Int] = {
    import OutputsWriter._

    insert(table, fieldsString, dataStrings(h, bt))
  }

  def proceedBlock(mod: ErgoFullBlock, xa: Transactor[IO]): IO[Int] = (for {
    hInt <- insertHeader(mod.header, mod.aDProofs, mod.blockTransactions)
    iInt <- insertInterlinks(mod.header)
    btInt <- insertBlockTransactions(mod.header, mod.blockTransactions)
    osInt <- insertOutputs(mod.header, mod.blockTransactions)
    isInt <- insertInputs(mod.header, mod.blockTransactions)
  } yield btInt + hInt + iInt + osInt + isInt).transact(xa)

  def readLastStats(xa: Transactor[IO]): IO[Option[StatRecord]] =
    Fragment.const(s"SELECT ${StatsWriter.fieldsString} from ${StatsWriter.table} ORDER BY ts DESC LIMIT 1")
      .query[StatRecord]
      .option
      .transact(xa)

  def writeStats(s: StatRecord, xa: Transactor[IO]): IO[Int] = {
    insert(StatsWriter.table, "(" + StatsWriter.fieldsString + ")", StatsWriter.statToInsertString(s)).transact(xa)
  }
}
