package com.github.kmizu.continuer

abstract class Processor[-In, +Out] {
  def name: String
  def process(input: In): Out
}
