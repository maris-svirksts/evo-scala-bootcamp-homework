package com.evolution.bootcamp.containers.integration
/*
import java.io.File

import com.dimafeng.testcontainers.{Container, DockerComposeContainer, ForAllTestContainer}
import com.redis.RedisClient
import com.redis.serialization.Parse.Implicits._
import com.redis.serialization.Format.default
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.must.Matchers.be
import org.scalatest.matchers.should.Matchers.convertToAnyShouldWrapper
import org.scalatest.verbs.ShouldVerb


class MailCounterSpec extends AnyFlatSpec with ShouldVerb with ForAllTestContainer  {
  override def container: Container = DockerComposeContainer( new File("integration/test-stack/docker-compose-redis.yml") )

  "Counter" should "persist value" in {
    val r = new RedisClient("localhost", 6379)
    val cntId = java.util.UUID.randomUUID().toString
//    val cntId = java.util.UUID.fromString("00000000-0000-0000-0000-000000000000").toString
    val c = new MailCounter(r, cntId)
    r.set(cntId, 0)
    c.addOne
    r.get[Long](cntId) should be (Some(1L))
  }

  container.stop()
}
 */
