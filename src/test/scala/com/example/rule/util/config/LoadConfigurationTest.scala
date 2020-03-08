package com.example.rule.util.config

import com.example.rule.pure.{Rule, ConfigSettings}
import pureconfig.ConfigReader.Result
import pureconfig.error.ConfigReaderFailures

class LoadConfigurationTest extends org.scalatest.FunSuite {

  val loaded = LoadConfiguration.rules



  test("has rules") {
    assert(
      LoadConfiguration.ruleConfig.hasPath("rules")
    )
  }

  test("rules ") {
    val res: List[Rule] =
      loaded match {
        case Left(value: ConfigReaderFailures) => Nil
        case Right(value) => value.rules
      }
   assert(res.head.name === "rule1")
  }
}
