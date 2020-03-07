package com.example.rule.pure

case class Rule(
                 name: String,
                 when: Option[String] = None,
                 `then`: String = "true",
                 rules: List[Rule] = Nil,
                 given: List[String] = Nil,
                 get: List[String] = Nil
               )
