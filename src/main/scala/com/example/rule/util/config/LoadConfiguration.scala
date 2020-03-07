package com.example.rule.util.config

import com.example.rule.pure.{Rule, ConfigSettings}
import com.typesafe.config.ConfigFactory
import pureconfig.ConfigReader.Result
import pureconfig.ConfigSource
import pureconfig._
import pureconfig.generic.auto._

object LoadConfiguration {
  val ruleConfig = ConfigFactory.load("rule.conf")

  val pureConfig = ConfigSource.fromConfig(ruleConfig)

  def rules: Result[ConfigSettings] = pureConfig.load[ConfigSettings]
}