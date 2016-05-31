package slacc
package ast

import utils._
import Trees._
import lexer._
import lexer.Tokens._

object Parser extends Pipeline[Iterator[Token], Program] {
  def run(ctx: Context)(tokens: Iterator[Token]): Program = {
    import ctx.reporter._


    /* A map containing a map of tokens at priority */
    val priorities = Map(   5 -> List(OR), 4 -> List(AND), 3 -> List(LESSTHAN,EQUALS),
                            2 -> List(PLUS,MINUS), 1 -> List(TIMES,DIV) ) // 0 indicates to do unary operators and then basic operations.


    /** Store the current token, as read from the lexer. */
    var currentToken: Token = new Token(BAD)

    def readToken: Unit = {
      if (tokens.hasNext) {
        // uses nextToken from the Lexer trait
        currentToken = tokens.next

        // skips bad tokens
        while (currentToken.kind == BAD) {
          currentToken = tokens.next
        }
      }
    }

    /** ''Eats'' the expected token, or terminates with an error. */
    def eat(kind: TokenKind): Unit = {
      if (currentToken.kind == kind) {
        readToken
      } else {
        expected(kind)
      }
    }

    /** Complains that what was found was not expected. The method accepts arbitrarily many arguments of type TokenKind */
    def expected(kind: TokenKind, more: TokenKind*): Nothing = {
      fatal("expected: " + (kind::more.toList).mkString(" or ") + ", found: " + currentToken, currentToken)
    }

    //==== ================ ====
    //==== BEGIN PARSE CODE ====
    //==== ================ ====

    def parseGoal: Program = {
      var lst = List[ClassDecl]()
      while(currentToken.kind == CLASS) { lst = lst :+ parseClass } 
      val main = parseMethod; eat(EOF) ;Program(MainMethod(main),lst).setPos(main)
    }

    def parseClass: ClassDecl = {
      var vars = List[VarDecl](); var methods = List[MethodDecl]()
      val p = currentToken;
      eat(CLASS);
      val name   = parseIdentifier;

      val generic = if(currentToken.kind == LBRACKET) {
                      eat(LBRACKET); val e = parseIdentifier; eat(RBRACKET); Some(e) }
                    else None

      val parent = if(currentToken.kind == LESSTHAN) {eat(LESSTHAN);eat(COLON); Some(parseIdentifier)}
                   else None

      eat(LBRACE);
      while(currentToken.kind == VAR) { vars = vars :+ parseVar }
      while(currentToken.kind == METHOD) { methods = methods :+ parseMethod }
      eat(RBRACE)

      ClassDecl(Identifier(name.value,generic).setPos(name),generic,parent,vars,methods).setPos(p)
    }

    def parseVar: VarDecl = {
      val p = currentToken
      eat(VAR); val id = parseIdentifier; eat(COLON); val t = parseType;
      eat(SEMICOLON); VarDecl(t,id).setPos(p)
    }

    def parseMethod: MethodDecl = {
      val p = currentToken;
      eat(METHOD); val name = parseIdentifier; eat(LPAREN);
      var args = List[Formal](); var vars = List[VarDecl]();
      var exprs = List[ExprTree](); //had parseExpression inside.

      while(currentToken.kind != RPAREN) {
        if(!args.isEmpty) eat(COMMA);
        val id = parseIdentifier;
        eat(COLON);
        val kind = parseType;
        args = args :+ Formal(kind,id).setPos(id);
      }

      eat(RPAREN); eat(COLON); val ret = parseType;
      eat(EQSIGN); eat(LBRACE);

      while(currentToken.kind == VAR) { vars = vars :+ parseVar }
      do { 
        if (!exprs.isEmpty) eat(SEMICOLON);
        exprs = exprs :+ parseExpression
      } while(currentToken.kind != RBRACE)

      eat(RBRACE); MethodDecl(ret,name,args,vars,exprs.init,exprs.last).setPos(p)
    }

    def parseType: TypeTree = {
      val p = currentToken;

      val t = currentToken match {
        case Kinded(BOOLEAN) => eat(BOOLEAN); BooleanType(); 
        case Kinded(STRING) =>  eat(STRING); StringType(); 
        case Kinded(UNIT) => eat(UNIT); UnitType();
        case cd: ID =>  eat(IDKIND); if(currentToken.kind == LBRACKET) { eat(LBRACKET); val tpe = parseType; eat(RBRACKET); Identifier(cd.value,Some(tpe)) } else Identifier(cd.value)
        case Kinded(INT) => { eat(INT);
          if (currentToken.kind == LBRACKET) { eat(LBRACKET); eat(RBRACKET); IntArrayType()}
          else { IntType() }
        }
        case _ => expected(BOOLEAN,STRING,UNIT,IDKIND,INT);
      }
      t.setPos(p)
    }

    def parseExpression: ExprTree = {
      parseExprDown(5)
    }

    def makeExpr(kind: TokenKind, lhs: ExprTree, rhs: ExprTree):ExprTree = {
      val v = kind match {
        case AND     => And(lhs,rhs)
        case OR      => Or(lhs,rhs)
        case PLUS    => Plus(lhs,rhs)
        case MINUS   => Minus(lhs,rhs)
        case TIMES   => Times(lhs,rhs)
        case DIV     => Div(lhs,rhs)
        case LESSTHAN=> LessThan(lhs,rhs)
        case EQUALS  => Equals(lhs,rhs)
        case _ => expected(AND,OR,PLUS,MINUS,TIMES,DIV,LESSTHAN,EQUALS)
      }
      v.setPos(lhs)
    }

    /*Start with the Lowest Priority Rule
      We have: ||, &&, ( < , == ), (- +), ( *, /), !, ([ and .) */
    def parseExprDown(i: Int): ExprTree = {
      if(i <= 0) {parseExprNot}
      else {
        var lhs = parseExprDown(i-1)
        while(priorities(i).contains(currentToken.kind)) {
          val k = currentToken.kind; eat(k);
          lhs = makeExpr(k,lhs,parseExprDown(i-1))
        }
        lhs
      }
    }

    def parseExprNot: ExprTree = {
      val p = currentToken;
      currentToken match {
        case Kinded(BANG) => eat(BANG); Not(parseExprNot).setPos(p)
        case _ => parseArray.setPos(p)
      }
    }

    def parseArray: ExprTree = { 
      var acc = parseDot;
      while( currentToken.kind == LBRACKET) {
        eat(LBRACKET); val idx = parseExpression; eat(RBRACKET);
        acc = ArrayRead(acc,idx).setPos(acc);
      }
      acc
    }
 
    def parseDot: ExprTree = {
      var acc = parseBasic;
      while(currentToken.kind == DOT) {
        eat(DOT);
        currentToken match {
          case Kinded(LENGTH) => eat(LENGTH); acc = ArrayLength(acc).setPos(acc);
          case ct: ID => val id = parseIdentifier;
            eat(LPAREN); var lst = List[ExprTree]();
            while(currentToken.kind != RPAREN) {
              if(!lst.isEmpty) eat(COMMA);
              lst = lst :+ parseExpression;
            }
            eat(RPAREN); acc = MethodCall(acc,id,lst).setPos(acc);
        }
      }
      acc
    }

    //VALIDATED UP TO HERE.

    def parseBasic: ExprTree = {
      val p = currentToken;
      currentToken match {
        case currentToken: INTLIT => eat(INTLITKIND); IntLit(currentToken.value).setPos(p);
        case currentToken: STRLIT => eat(STRLITKIND); StringLit(currentToken.value).setPos(p);
        case Kinded(TRUE) => eat(TRUE); True().setPos(p);
        case Kinded(FALSE) => eat(FALSE); False().setPos(p);
        case Kinded(SELF) => eat(SELF); Self().setPos(p);
        case Kinded(NEW) => {
          eat(NEW);
          if(currentToken.kind == INT) {eat(INT); eat(LBRACKET);
            val e = parseExpression; eat(RBRACKET); NewIntArray(e).setPos(p)
          } else {
            val v = parseType;
            v match {
            case i : Identifier => eat(LPAREN); eat(RPAREN); New(i).setPos(p)
            case _ => fatal("expected non-primitive type",p)
            }
          }
        }
        case Kinded(LPAREN) => eat(LPAREN); val v = parseExpression; eat(RPAREN); v.setPos(p);
        case Kinded(LBRACE) => {
          eat(LBRACE);
          var lst = List[ExprTree]();
          while(currentToken.kind != RBRACE) {
            if(!lst.isEmpty) eat(SEMICOLON);
            lst = lst :+ parseExpression;
          }
          eat(RBRACE); Block(lst).setPos(p)
        }
        case Kinded(IF) => {eat(IF); eat(LPAREN); val e = parseExpression; eat(RPAREN);
          val s = parseExpression;
          val f = if(currentToken.kind == ELSE){eat(ELSE); Some(parseExpression)} else None
          If(e,s,f).setPos(p)
        }
        case Kinded(WHILE) => { eat(WHILE); eat(LPAREN); val e = parseExpression; eat(RPAREN);
          While(e,parseExpression).setPos(p);
        }
        case Kinded(PRINTLN) => { eat(PRINTLN); eat(LPAREN); val e = parseExpression; eat(RPAREN);
          Println(e).setPos(p)
        }
        case Kinded(STROF) => { eat(STROF); eat(LPAREN); val e = parseExpression; eat(RPAREN);
          Strof(e).setPos(p)
        }
        case Kinded(IDKIND) => {
          val id = parseIdentifier;
          currentToken match {
            case Kinded(EQSIGN) => eat(EQSIGN); Assign(id,parseExpression).setPos(p)
            case Kinded(LBRACKET) => eat(LBRACKET); val idx = parseExpression; eat(RBRACKET);
              if(currentToken.kind == EQSIGN) { eat(EQSIGN); ArrayAssign(id,idx,parseExpression).setPos(p) }
              else ArrayRead(id,idx).setPos(p)
            case _ => id
          }
        }
        case _ => {
            expected(INTLITKIND,STRLITKIND,TRUE,FALSE,SELF,NEW,LPAREN,LBRACE,IF,WHILE,PRINTLN,STROF,IDKIND);
        }
      }
    }

    def parseIdentifier: Identifier = {
        val v = currentToken match {
            case currentToken: ID => new Identifier(currentToken.value).setPos(currentToken)
            case _ => expected(IDKIND)
        }
        eat(IDKIND); v
    }


    readToken
    val tree = parseGoal
    terminateIfErrors
    tree
  }
}
