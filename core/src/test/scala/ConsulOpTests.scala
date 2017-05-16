package helm

import argonaut._, Argonaut._
import scalaz.\/
import scalaz.concurrent.Task
import scalaz.concurrent.Task.{delay, now}
import org.scalatest.{FlatSpec, Matchers}
import org.scalactic.TypeCheckedTripleEquals
import ConsulOp._

class ConsulOpTests extends FlatSpec with Matchers with TypeCheckedTripleEquals {
  val I = Interpreter.prepare[ConsulOp, Task]

  "getJson" should "return none right when get returns None" in {
    val interp = for {
      _ <- I.expectU[Option[String]] {
        case ConsulOp.Get("foo") => now(None)
      }
    } yield ()
    interp.run(getJson[Json]("foo")).run should equal(\/.right(None))
  }

  it should "return a value when get returns a decodeable value" in {
    val interp = for {
      _ <- I.expectU[Option[String]] {
        case ConsulOp.Get("foo") => now(Some("42"))
      }
    } yield ()
    interp.run(getJson[Json]("foo")).run should equal(\/.right(Some(jNumber(42))))
  }

  it should "return an error when get returns a non-decodeable value" in {
    val interp = for {
      _ <- I.expectU[Option[String]] {
        case ConsulOp.Get("foo") => now(Some("{"))
      }
    } yield ()
    interp.run(getJson[Json]("foo")).run should equal(\/.left("JSON terminates unexpectedly."))
  }

  "healthCheck" should "return a vector of health status values when decodeable" in {
    import HealthStatus._
    val interp = for {
      _ <- I.expectU[String] {
        case ConsulOp.HealthCheck("foo") => now("""[{"Status":"passing"},{"Status":"warning"}]""")
      }
    } yield ()
    interp.run(healthCheckJson[HealthStatus]("foo")).run should equal(\/.right(List(Passing,Warning)))
  }

  it should "return a error if status id not decodeable" in {
    import HealthStatus._
    val interp = for {
      _ <- I.expectU[String] {
        case ConsulOp.HealthCheck("foo") => now("""[{"Status":"bar"}]""")
      }
    } yield ()
    interp.run(healthCheckJson[HealthStatus]("foo")).run.isLeft should equal(true)
  }

  it should "return a empty set if response is empty" in {
    import HealthStatus._
    val interp = for {
      _ <- I.expectU[String] {
        case ConsulOp.HealthCheck("foo") => now("""[]""")
      }
    } yield ()
    interp.run(healthCheckJson[HealthStatus]("foo")).run should equal(\/.right(List()))
  }

  "catalogServices" should "return a vector of registered services" in {
    val interp = for {
      _ <- I.expectU[Option[String]] {
        case ConsulOp.GetCatalogServices => now(Some("""{"consul":[],"service-name":["tag1","tag2"]}"""))
      }
    } yield ()
    interp.run(getJson[Json]("service-name")).run should equal(\/.right(Some(List("tag1","tag2"))))
  }

  it should "get the details of a service" in {
    val mockServiceResponse = """[
                                |  {
                                |    "ID": "25867a8d-d37e-4334-aead-cf85337b1909",
                                |    "Node": "consul.docker",
                                |    "Address": "172.18.0.3",
                                |    "TaggedAddresses": {
                                |      "lan": "172.18.0.3",
                                |      "wan": "172.18.0.3"
                                |    },
                                |    "NodeMeta": {},
                                |    "ServiceID": "service-name-tag1-8888",
                                |    "ServiceName": "service-name",
                                |    "ServiceTags": [
                                |      "tag1",
                                |      "tag2"
                                |    ],
                                |    "ServiceAddress": "",
                                |    "ServicePort": 8888,
                                |    "ServiceEnableTagOverride": false,
                                |    "CreateIndex": 63567,
                                |    "ModifyIndex": 63567
                                |  }
                                |]""".stripMargin
//    val interp = for {
//      _ <- I.expectU[Option[String]] {
//        case ConsulOp.GetService("service-name") => now(Some(mockServiceResponse))
//      }
//    } yield ()
//    interp.run(???) // How to decode an array?
  }
}
