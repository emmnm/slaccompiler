package slacc
package utils

import java.io.File

case class Context(
  val reporter: Reporter,
  val files: List[File] = Nil,
  val outDir: Option[File] = None,
  val doEval: Boolean = false,
  val doHelp: Boolean = false,
  val doPrintMain: Boolean = false,
  val doTokens: Boolean = false,
  val doAST: Boolean = false,
  val doSymbolIds: Boolean = false
)
