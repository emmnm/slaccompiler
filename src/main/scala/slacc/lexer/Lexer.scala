package slacc
package lexer

import utils._
import scala.io.Source
import java.io.File

object Lexer extends Pipeline[File, Iterator[Token]] {
  import Tokens._

  def run(ctx: Context)(f: File): Iterator[Token] = {
    val source = Source.fromFile(f)
    var done = false

    import ctx.reporter._

    // Make map from Char to Token Type. If in here, then check.
    val singles = Map(':' -> COLON, ';' -> SEMICOLON, '.' -> DOT, ',' -> COMMA, '!' -> BANG,
        '(' -> LPAREN, ')' -> RPAREN, '[' -> LBRACKET, ']' -> RBRACKET, '{' -> LBRACE, '}' -> RBRACE,
        '<' -> LESSTHAN, '+' -> PLUS, '-' -> MINUS, '*' -> TIMES)

    val keywords = Map("class" -> CLASS, "method" -> METHOD, "var" -> VAR, "Unit" -> UNIT,
        "String" -> STRING, "extends" -> EXTENDS, "Int" -> INT, "Bool" -> BOOLEAN, "while" -> WHILE,
        "if" -> IF, "else" -> ELSE, "length" -> LENGTH, "true" -> TRUE, "false" -> FALSE,
        "self" -> SELF, "new" -> NEW, "println" -> PRINTLN, "strOf" -> STROF)

    val doubles = Map('&' -> AND, '|' -> OR);

    def remLine() { /* Assumes you saw a //, will remove everything until newline */
        while(source.hasNext && source.ch != '\n') { source.next }
        if(source.hasNext) source.next;
    }

    def remMultiLine(): Boolean = { /* just saw a /*   so we have to break iff  */ is in stream. */
        var past = source.ch;
        while((source.ch != '/' || past != '*')) {
          if(!source.hasNext) return false;
          past = source.ch; source.next
        }
        if(source.hasNext) source.next;
        return true;
    }    

    def remWhitespace() {
        while(source.hasNext && source.ch.isWhitespace) {source.next }
    }

   source.next

    new Iterator[Token] {

      def hasNext = {
        !done
      }

      def next = {

        remWhitespace

        val pos = source.pos
        var token = new Token(BAD)

        /* If no characters left, return EOF. */
        if(!source.hasNext) {
            token = new Token(EOF).setPos(f,pos)
            done = true

        } else if(source.ch.isLetter) { /* Eat up as long as letter, digit, or _ */
            val buf = new StringBuilder
            while(source.ch.isLetterOrDigit || source.ch == '_') { buf += source.ch; source.next }
            val s = buf.toString
            token = 
                if(keywords.contains(s)) new Token(keywords(s)).setPos(f,pos)
                else new ID(s).setPos(f,pos)

        } else if(source.ch == '"') { /* Get everything in between the quotes */
            val buf = new StringBuilder
            source.next
            while(source.ch != '\n' && source.ch != '"') { buf += source.ch; source.next }
            source.next
            token = new STRLIT(buf.toString).setPos(f,pos)

        } else if (source.ch.isDigit) { /* Integer Literal */
            if (source.ch == '0') {        /* if 0, must be only one. */
                token = new INTLIT(0).setPos(f,pos)
                source.next
            } else {
                val buf = new StringBuilder
                while(source.ch.isDigit) { buf += source.ch; source.next }
                token = new INTLIT(buf.toString.toInt).setPos(f,pos)
            }
        } else if (singles.contains(source.ch) ) { /* Any single character tokens here */
            token = new Token(singles(source.ch)).setPos(f,pos); source.next;

        } else if (doubles.contains(source.ch) ) { /* Any double only characters here */
            val c = source.ch;
            token = if (source.next == c) {source.next; new Token(doubles(c)).setPos(f,pos) }
                    else new Token(BAD).setPos(f,pos)

        } else if (source.ch == '=') { /* equal signs being handled. multiple cases. */
            token = if(source.next == '=') {source.next; new Token(EQUALS).setPos(f,pos) }
                    else new Token(EQSIGN).setPos(f,pos)

        } else if (source.ch == '/') { /*Divides and comments being handled here */
            val n = source.next
            token = if(n == '*') { if(!remMultiLine) {new Token(BAD).setPos(f,pos); } else next } /* Multi-line comment */
                    else if(n == '/') { remLine; next } /* Single Line Comment */
                    else { new Token(DIV).setPos(f,pos) } /* Divides */

        } else { /* Bad token, continue on */
            source.next; token = token.setPos(f,pos)
        }


        if(token.kind == BAD) {
          Console.err.println("Token Error at location " + "(" + token.line + ":" + token.col + ")");
        }

        /* Remove any comments if applicable */
       // if(source.hasNext) {
                //remove any whitespace, comments here to next legal character.
                //source.next
       //         while(source.ch.isWhitespace) { source.next }
       // }

        remWhitespace

        token
      }
    }
  }
}

object DisplayTokens extends Pipeline[Iterator[Token], Iterator[Token]] {
  def run(ctx: Context)(tokens: Iterator[Token]): Iterator[Token] = {
    new Iterator[Token] {
      def hasNext = {
        tokens.hasNext
      }

      def next = {
        val n = tokens.next
        println(n+"("+n.line+":"+n.col+") ")
        n
      }
    }
  }
}
