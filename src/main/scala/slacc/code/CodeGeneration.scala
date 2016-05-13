package slacc
package code

import ast.Trees._
import analyzer.Symbols._
import analyzer.Types._
import cafebabe._
import AbstractByteCodes.{New => _, _}
import ByteCodes._
import utils._

object CodeGeneration extends Pipeline[Program, Unit] {

  def run(ctx: Context)(prog: Program): Unit = {
    import ctx.reporter._

    def generateTypeString( t : Type ) : String = {
      t match {
        case TInt => "I"
        case TBoolean => "Z"
        case TIntArray => "[I"
        case TString => "Ljava/lang/String;"
        case TUnit => "V"
        case TObject(_) => "L" + t.toString + ";"
        case _ => System.err.println("Invalid Type in Code Generation"); "V"
      }
    }

    def generateMainClassFile(sourceName: String, mm : MainMethod, dir : String): Unit = {
      val classFile = new ClassFile(mm.getSymbol.name,None)
      classFile.setSourceFile(sourceName)
      classFile.addDefaultConstructor

      val mainHandler = classFile.addMainMethod
      generateMethodCode(mainHandler.codeHandler,mm.main)

      classFile.writeToFile(dir + mm.getSymbol.name + ".class")
    }

    /** Writes the proper .class file in a given directory. An empty string for dir is equivalent to "./". */
    def generateClassFile(sourceName: String, ct: ClassDecl, dir: String): Unit = {

      val parent = ct.parent match { case Some(p) => Some(p.value) case None => None }

      val classFile = new ClassFile(ct.id.value,parent)
      classFile.setSourceFile(sourceName)
      classFile.addDefaultConstructor

      ct.vars.foreach(x => {
        val sym = x.getSymbol
        classFile.addField(generateTypeString(sym.getType),sym.name)
      })

      ct.methods.foreach( x => {
        val ch : CodeHandler = classFile.addMethod(
                generateTypeString(x.retType.getType),
                x.id.value,
                x.args.map(x => generateTypeString(x.tpe.getType)).mkString("")
            ).codeHandler
        generateMethodCode(ch,x)
      })

      //TODO: Store line numbers / src file id (see cafebabe.

      classFile.writeToFile(dir + ct.id.value + ".class")
    }

    // a mapping from variable symbols to positions in the local variables
    // of the stack frame
    def generateMethodCode(ch: CodeHandler, mt: MethodDecl): Unit = {
      val methSym = mt.getSymbol
      val classSym = methSym.classSymbol


      val offset = if( classSym == prog.main.getSymbol) 0 else 1
      val formals =  mt.args.foldLeft(Map[Symbol,Int]() )(
        (m, x) => { m + (x.getSymbol -> (m.size + offset)) })
      val locals  =  mt.vars.foldLeft(Map[Symbol,Int]() )((m, x) => {m + (x.getSymbol -> ch.getFreshVar )})
      val ordering = formals ++ locals

      def slotFor(e: Symbol ) : Int = {
        ordering.getOrElse(e,-1)
      }

      def generateExprCode(expr: ExprTree): Unit = {
        expr match {

          case e : And => { val s_skip = ch.getFreshLabel("skip")
            ch << ICONST_0; generateExprCode(e.lhs);
            ch << IfEq(s_skip); ch << POP; generateExprCode(e.rhs); ch << Label(s_skip)
          }
          case e : Or => { val s_skip = ch.getFreshLabel("skip")
            ch << ICONST_1; generateExprCode(e.lhs);
            ch << IfNe(s_skip); ch << POP; generateExprCode(e.rhs); ch << Label(s_skip)
          }
          case e : Plus => {
            e.getType match {
              case TInt => generateExprCode(e.lhs); generateExprCode(e.rhs); ch << IADD
              case TString => {
                  ch << DefaultNew("java/lang/StringBuilder")
                  generateExprCode(e.lhs)
                  ch << InvokeVirtual("java/lang/StringBuilder","append",
                    "(" + generateTypeString(e.lhs.getType) + ")Ljava/lang/StringBuilder;")
                  generateExprCode(e.rhs)
                  ch << InvokeVirtual("java/lang/StringBuilder","append",
                    "(" + generateTypeString(e.rhs.getType) + ")Ljava/lang/StringBuilder;")
                  ch << InvokeVirtual("java/lang/StringBuilder","toString","()Ljava/lang/String;")
              }
              case _ => System.err.println("Internal Error: Invalid Plus Operator Types")
            }
          }
          case e : Minus => generateExprCode(e.lhs); generateExprCode(e.rhs); ch << ISUB
          case e : Times => generateExprCode(e.lhs); generateExprCode(e.rhs); ch << IMUL
          case e : Div => generateExprCode(e.lhs); generateExprCode(e.rhs); ch << IDIV
          case e : LessThan => ch << ICONST_1; 
                generateExprCode(e.lhs); generateExprCode(e.rhs);
                val s = ch.getFreshLabel("lt")
                ch << If_ICmpLt(s) << POP << ICONST_0 << Label(s)

          case e : Equals => { val s_cond = ch.getFreshLabel("cond")
            ch << ICONST_1
            generateExprCode(e.lhs); generateExprCode(e.rhs)
            (e.lhs.getType,e.rhs.getType) match {
              case (TInt,TInt) => ch << If_ICmpEq(s_cond)
              case (TBoolean, TBoolean) => ch << If_ICmpEq(s_cond)
              case _ => ch << If_ACmpEq(s_cond)
            }
            ch << POP << ICONST_0 << Label(s_cond)
          }



          case e : ArrayRead => generateExprCode(e.arr); generateExprCode(e.index); ch << IALOAD
          case e : ArrayLength => generateExprCode(e.arr); ch << ARRAYLENGTH;
          case e : MethodCall => {
            generateExprCode(e.obj);  e.args.foreach{ generateExprCode }
            e.obj.getType match {
              case TObject(o) => 
                o.lookupMethod(e.meth.value) match {
                  case Some(jkl) =>
                    ch << InvokeVirtual(o.name,e.meth.value,
                    jkl.argList.map(x => generateTypeString(x.getType)).mkString("(","",")") + generateTypeString(e.getType))
                  case None => System.err.println("internal error")
                }

              case _ => System.err.println("Internal Error")
            }
          }
          case e : IntLit => ch << Ldc(e.value)
          case e : StringLit => ch << Ldc(e.value)
          case e : True => ch << ICONST_1
          case e : False => ch << ICONST_0
          case e : Identifier => // no shadowing.
            slotFor(e.getSymbol) match {
                case -1 => ch << ALoad(0) << GetField(classSym.name,e.getSymbol.name,generateTypeString(e.getSymbol.getType))
                case n => e.getType match {
                  case TInt => ch << ILoad( n )
                  case TBoolean => ch << ILoad( n )
                  case TIntArray => ch << ALoad( n )
                  case TString => ch << ALoad( n )
                  case TObject(a) => ch << ALoad( n )
                  case _ => System.err.println("Internal Error: Wrong Type Value for Identifier")
                }
            }
          case e : Self => ch << ALoad(0)
          case e : NewIntArray => generateExprCode(e.size); ch << NewArray(10) //T_INT=10
          case e : New => e.tpe.getType match {
            case TObject(o) => ch << DefaultNew(o.name)
            case _ => System.err.println("Internal Error.")
          }
          case e : Not => { val s_cond = ch.getFreshLabel("cond")
            generateExprCode(e.expr); ch << ICONST_1 << SWAP << IfEq(s_cond)
            ch << POP << ICONST_0 << Label(s_cond)
          }
          case e : Block => if(e.exprs.length == 0) TUnit
            else {
              e.exprs.take(e.exprs.length-1).foreach(x => {
              generateExprCode(x);
              if(x.getType != TUnit) {
                ch << POP;
              }
            }); generateExprCode(e.exprs.last);
          }
          case e : If => {
            val s_els = ch.getFreshLabel("els")
            val s_don = ch.getFreshLabel("don")
            generateExprCode(e.expr)
            e.els match {
              case Some(els) => ch << IfEq(s_els); generateExprCode(e.thn);
                ch << Goto(s_don) << Label(s_els);
                generateExprCode(els); ch << Label(s_don)
              case None => ch << IfEq(s_don); generateExprCode(e.thn);
                if(e.thn.getType != TUnit) ch << POP; ch << Label(s_don)
            }
          }
          case e : While => {
            val s_cond = ch.getFreshLabel("cond")
            val s_don = ch.getFreshLabel("done")
            ch << Label(s_cond); generateExprCode(e.cond); ch << IfEq(s_don);
            generateExprCode(e.body);
            ch << Goto(s_cond) << Label(s_don);
          }
          case e : Println => {
            ch << GetStatic("java/lang/System","out","Ljava/io/PrintStream;")
            generateExprCode(e.expr)
            ch << InvokeVirtual("java/io/PrintStream", "println", "(Ljava/lang/String;)V")
          }
          case e : Assign => {
            slotFor(e.id.getSymbol) match {
              case -1 => ch << ALoad(0);
                  generateExprCode(e.expr);
                  ch << PutField(classSym.name,e.id.value,generateTypeString(e.id.getSymbol.getType))
              case n => generateExprCode(e.expr); e.id.getType match {
                  case TInt => ch << IStore( n )
                  case TBoolean => ch << IStore( n )
                  case TIntArray => ch << AStore( n )
                  case TString => ch << AStore( n )
                  case TObject(_) => ch << AStore( n )
                  case _ => System.err.println("Internal Error")
              }
            }
          }
          case e : ArrayAssign => {
            classSym.lookupVar(e.id.value) match {
              case Some(a) => ch << ALoad(0);
                ch << GetField(classSym.name,e.id.value,generateTypeString(a.getType))
                generateExprCode(e.index);
                generateExprCode(e.expr);
                ch << IASTORE
              case None => ch << ALoad(slotFor(e.id.getSymbol))
                generateExprCode(e.index)
                generateExprCode(e.expr)
                ch << IASTORE
            }
          }
          case e : Strof => {
            ch << DefaultNew("java/lang/StringBuilder")
            generateExprCode(e.expr)
            ch << InvokeVirtual("java/lang/StringBuilder","append",
                    "(" + generateTypeString(e.expr.getType) + ")Ljava/lang/StringBuilder;")
            ch << InvokeVirtual("java/lang/StringBuilder","toString","()Ljava/lang/String;")
          }
        }
      }

      //exprs
      mt.exprs.foreach(x => {
        generateExprCode(x)
        if(x.getType != TUnit)  
          ch << POP;
      })

      //ret expr
      generateExprCode(mt.retExpr)

      //retType
      mt.retType.getType match {
        case TInt => ch << IRETURN
        case TBoolean => ch << IRETURN
        case TIntArray => ch << ARETURN
        case TString => ch << ARETURN
        case TUnit => ch << RETURN
        case TObject(_) => ch << ARETURN
        case _ => System.err.println("Internal Error")
      }
      ch.freeze
    }

    val outDir = ctx.outDir.map(_.getPath+"/").getOrElse("./")

    val f = new java.io.File(outDir)
    if (!f.exists()) {
      f.mkdir()
    }

    val sourceName = ctx.files.head.getName

    // output code
    prog.classes foreach {
      ct => generateClassFile(sourceName, ct, outDir)
    }

    generateMainClassFile(sourceName,prog.main,outDir)

  }

}
