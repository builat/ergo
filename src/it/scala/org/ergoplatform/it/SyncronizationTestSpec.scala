package org.ergoplatform.it

import org.scalatest.{FreeSpec, Matchers}
import org.scalatest.concurrent.{IntegrationPatience, ScalaFutures}
import scorex.core.utils.ScorexLogging

import scala.concurrent.Await.result
import scala.concurrent.Future.traverse
import scala.concurrent.duration._
import scala.concurrent.ExecutionContext.Implicits.global

class SyncronizationTestSpec(nodes: Seq[Node]) extends FreeSpec with ScalaFutures with IntegrationPatience
  with Matchers with ScorexLogging {

  val cnt = 30

  s"Generate $cnt blocks" in {
    val headerIdsAtSameHeight = result(for {
      b <- traverse(nodes)(_.height).map(_.min)
      _ <- traverse(nodes)(_.waitForHeight(b + cnt))
      headers <- traverse(nodes)(_.headerIdsByHeight(b + cnt - 1))
    } yield {
      log.debug(s"Headers at height ${b + cnt - 1}: ${headers.mkString(",")}")
      headers
    }, 10.minutes)

    val someHeaderId = headerIdsAtSameHeight.head.head
    all(headerIdsAtSameHeight) should contain(someHeaderId)
  }
}

