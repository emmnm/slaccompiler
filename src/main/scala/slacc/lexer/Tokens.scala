package slacc
package lexer

import utils._

sealed class Token(val kind: TokenKind) extends Positioned {
  override def toString = kind.toString
}

sealed trait TokenKind;

object Tokens {

  object STRLITKIND extends TokenKind {
    override def toString = "string literal"
  }

  object INTLITKIND extends TokenKind {
    override def toString = "integer literal"
  }

  object IDKIND extends TokenKind {
    override def toString = "identifier"
  }

  // This allows you to do:
  // myToken match {
  //   case Kinded(IDKIND) => 
  //  ...
  // }
  object Kinded {
    def unapply(t: Token): Option[TokenKind] = {
      Some(t.kind)
    }
  }

  case object BAD extends TokenKind         // represents incorrect tokens.
  case object EOF extends TokenKind
  case object COLON extends TokenKind       // :
  case object SEMICOLON extends TokenKind   // ;
  case object DOT extends TokenKind         // .
  case object COMMA extends TokenKind       // ,
  case object EQSIGN extends TokenKind      // =
  case object EQUALS extends TokenKind      // ==
  case object BANG extends TokenKind        // !
  case object LPAREN extends TokenKind      // (
  case object RPAREN extends TokenKind      // )
  case object LBRACKET extends TokenKind    // [
  case object RBRACKET extends TokenKind    // ]
  case object LBRACE extends TokenKind      // {
  case object RBRACE extends TokenKind      // }
  case object AND extends TokenKind         // &&
  case object OR extends TokenKind          // ||
  case object LESSTHAN extends TokenKind    // <
  case object PLUS extends TokenKind        // +
  case object MINUS extends TokenKind       // -
  case object TIMES extends TokenKind       // *
  case object DIV extends TokenKind         // /
  case object CLASS extends TokenKind       // class
  case object METHOD extends TokenKind      // method
  case object VAR extends TokenKind         // var
  case object UNIT extends TokenKind        // unit
  case object STRING extends TokenKind      // string
  case object EXTENDS extends TokenKind     // extends
  case object INT extends TokenKind         // int
  case object BOOLEAN extends TokenKind     // boolean
  case object WHILE extends TokenKind       // while
  case object IF extends TokenKind          // if
  case object ELSE extends TokenKind        // else
  case object LENGTH extends TokenKind      // length
  case object TRUE extends TokenKind        // true
  case object FALSE extends TokenKind       // false
  case object SELF extends TokenKind        // self
  case object NEW extends TokenKind         // new
  case object PRINTLN extends TokenKind     // println
  case object STROF extends TokenKind       // strOf

  // Identifiers
  class ID(val value: String) extends Token(IDKIND) {
    override def toString = "ID("+value+")"
  }

  // Integer literals
  class INTLIT(val value: Int) extends Token(INTLITKIND) {
    override def toString = "INT("+value+")"
  }

  // String literals
  class STRLIT(val value: String) extends Token(STRLITKIND) {
    override def toString = "STR("+value+")"
  }
}
