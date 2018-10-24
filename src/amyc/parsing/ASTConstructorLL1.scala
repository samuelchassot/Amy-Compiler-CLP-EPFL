package amyc
package parsing

import grammarcomp.parsing._
import utils.Positioned
import ast.NominalTreeModule._
import Tokens._
import amyc.ast.NominalTreeModule

// Implements the translation from parse trees to ASTs for the LL1 grammar,
// that is, this should correspond to Parser.amyGrammarLL1.
// We extend the plain ASTConstructor as some things will be the same -- you should
// override whatever has changed. You can look into ASTConstructor as an example.
class ASTConstructorLL1 extends ASTConstructor {

  @Override
  override def constructQname(pTree: NodeOrLeaf[Token]): (QualifiedName, Positioned) = {
    pTree match{
      case Node('QName ::= _, List(nm, opt)) => {
        val (name, pos) = constructName(nm)
        opt match {
          case Node('QNameOpt ::= _, List(_, mod)) => {
            val (module, modPos) = constructName(mod)
            (QualifiedName(Some(module), name), modPos)
          }
          case Node('QNameOpt ::= _, List()) =>
            (QualifiedName(None, name), pos);

        }
      }
    }
  }

  @Override
  override def constructExpr(ptree: NodeOrLeaf[Token]): NominalTreeModule.Expr = {
    ptree match {
      case Node('Expr ::= _, List(lvl01)) => constructExprLvl01(lvl01)
    }
  }

  def constructExprLvl01(ptree: NodeOrLeaf[Token]): NominalTreeModule.Expr = {
    ptree match{
      case Node('lvl01 ::= (VAL() :: _), List(Leaf(vt), param, _, valueTree, _, expr)) => {
        Let(constructParam(param), constructExpr(valueTree), constructExpr(expr)).setPos(vt)
      }
      case Node('lvl01 ::= List('lvl02), List(lvl02)) =>
        constructExprLvl02(lvl02)
    }
  }
  def constructExprLvl02(ptree: NodeOrLeaf[Token]): NominalTreeModule.Expr = {
    ptree match {
      case Node('lvl02 ::= List('lvl03, 'lvl02opt), List(lvl03Tree, lvl02optTree)) =>{
        lvl02optTree match{
          case Node('lvl02Opt ::= (MATCH() :: _), List(Leaf(mt), _, casesTree, _)) =>
            Match(constructExprLvl03(lvl03Tree), constructList1(casesTree, constructCase))
          case Node('lvl02Opt ::= List(), List()) =>
            constructExprLvl03(lvl03Tree)
        }
      }
    }
  }

  def constructExprLvl03(ptree: NodeOrLeaf[Token]): NominalTreeModule.Expr = {
    ptree match {
      case Node('lvl03 ::= _, List(lvl04Tree, lvl03Opt)) => {
        lvl03Opt match{
          case Node('lvl03Opt ::= List(OR(), 'lvl03), List(ort, lvl03Tree)) =>
            constructOp(ort)(constructExprLvl04(lvl04Tree), constructExprLvl03(lvl03Tree))
          case Node('lvl03Opt ::= List(), List()) =>
            constructExprLvl04(lvl04Tree)
        }
      }
    }
  }

  def constructExprLvl04(ptree: NodeOrLeaf[Token]) : NominalTreeModule.Expr = {
    ptree match {
      case Node('lvl04 ::= _, List(lvl05Tree, lvl04Opt)) =>
        lvl04Opt match{
          case Node('lvl04Opt ::= List(AND(), 'lvl04), List(andt, lvl04Tree)) =>
            constructOp(andt)(constructExprLvl05(lvl05Tree), constructExprLvl04(lvl04Tree))
          case Node('lvl04Opt ::= List(), List()) =>
            constructExprLvl05(lvl05Tree)
        }
    }
  }

  def constructExprLvl05(ptree: NodeOrLeaf[Token]) : NominalTreeModule.Expr = {
    ptree match {
      case Node('lvl05 ::= _, List(lvl06Tree, lvl05Opt)) =>
        lvl05Opt match{
          case Node('lvl05Opt ::= List(EQUALS(), 'lvl05), List(eqt, lvl05Tree)) =>
            constructOp(eqt)(constructExprLvl06(lvl06Tree), constructExprLvl05(lvl05Tree))
          case Node('lvl05Opt ::= List(), List()) =>
            constructExprLvl06(lvl06Tree)
        }
    }
  }

  def constructExprLvl06(ptree: NodeOrLeaf[Token]) : NominalTreeModule.Expr = {
    ptree match {
      case Node('lvl06 ::= _, List(lvl07Tree, lvl06Opt)) =>
        lvl06Opt match{
          case Node('lvl06Opt ::= List(LESSTHAN(), 'lvl06), List(ltt, lvl06Tree)) =>
            constructOp(ltt)(constructExprLvl07(lvl07Tree), constructExprLvl06(lvl06Tree))
          case Node('lvl06Opt ::= List(LESSEQUALS(), 'lvl06), List(leqt, lvl06Tree)) =>
            constructOp(leqt)(constructExprLvl07(lvl07Tree), constructExprLvl06(lvl06Tree))
          case Node('lvl05Opt ::= List(), List()) =>
            constructExprLvl07(lvl07Tree)
        }
    }
  }

  def constructExprLvl07(ptree: NodeOrLeaf[Token]) : NominalTreeModule.Expr = {
    ptree match {
      case Node('lvl07 ::= _, List(lvl08Tree, lvl07Opt)) =>
        lvl07Opt match{
          case Node('lvl07Opt ::= List(PLUS(), 'lvl07), List(pt, lvl07Tree)) =>
            constructOp(pt)(constructExprLvl08(lvl08Tree), constructExprLvl07(lvl07Tree))
          case Node('lvl07Opt ::= List(MINUS(), 'lvl07), List(mt, lvl07Tree)) =>
            constructOp(mt)(constructExprLvl08(lvl08Tree), constructExprLvl07(lvl07Tree))
          case Node('lvl07Opt ::= List(CONCAT(), 'lvl07), List(ct, lvl07Tree)) =>
            constructOp(ct)(constructExprLvl08(lvl08Tree), constructExprLvl07(lvl07Tree))
          case Node('lvl07Opt ::= List(), List()) =>
            constructExprLvl08(lvl08Tree)
        }
    }
  }

  def constructExprLvl08(ptree: NodeOrLeaf[Token]) : NominalTreeModule.Expr = {
    ptree match {
      case Node('lvl08 ::= _, List(lvl09Tree, lvl08Opt)) =>
        lvl08Opt match{
          case Node('lvl08Opt ::= List(TIMES(), 'lvl08), List(tt, lvl08Tree)) =>
            constructOp(tt)(constructExprLvl09(lvl09Tree), constructExprLvl08(lvl08Tree))
          case Node('lvl08Opt ::= List(DIV(), 'lvl08), List(dt, lvl08Tree)) =>
            constructOp(dt)(constructExprLvl09(lvl09Tree), constructExprLvl08(lvl08Tree))
          case Node('lvl08Opt ::= List(MOD(), 'lvl08), List(modt, lvl08Tree)) =>
            constructOp(modt)(constructExprLvl09(lvl09Tree), constructExprLvl08(lvl08Tree))
          case Node('lvl08Opt ::= List(), List()) =>
            constructExprLvl09(lvl09Tree)
        }
    }
  }

  def constructExprLvl09(ptree: NodeOrLeaf[Token]) : NominalTreeModule.Expr = {
    ptree match {
      case Node('lvl09 ::= List(BANG(), 'lvl10), List(Leaf(bt), lvl10Tree)) =>
        Not(constructExprLvl10(lvl10Tree))
      case Node('lvl09 ::= List(MINUS(), 'lvl10), List(Leaf(mt), lvl10Tree)) =>
        Neg(constructExprLvl10(lvl10Tree))
      case Node('lvl10 ::= List('lvl10), List(lvl10Tree)) =>
        constructExprLvl10(lvl10Tree)

    }
  }

  def constructExprLvl10(ptree: NodeOrLeaf[Token]) : NominalTreeModule.Expr = {
    ptree match {
      case Node('lvl10 ::= (IF()::_), List(Leaf(it), _, cond, _, _, thenn, _, _, _, elze, _)) =>
        Ite(
          constructExpr(cond),
          constructExpr(thenn),
          constructExpr(elze)
        ).setPos(it)
      case Node('lvl10 ::= (ERROR() :: _), List(Leaf(ert), _, msg, _)) =>
        Error(constructExpr(msg)).setPos(ert)
      case Node('lvl10 ::= List('Id, 'IdExprOpt), List(id, idOpt)) => {
        idOpt match {
          case Node('IdExprOpt ::= ('QNameOpt ::_), List(qnameopt, _, argsTree, _)) =>{
            qnameopt match{
              case Node('QNameOpt ::= (DOT() :: _), List(_, nameTree)) =>{
                val (module, modPos) = constructName(id)
                val (name, namePos) = constructName(nameTree)
                val qname = QualifiedName(Some(module), name)
                val args = constructList(argsTree, constructExpr, hasComma = true)
                Call(qname, args).setPos(modPos)
              }
              case Node('QNameOpt ::= List(), List()) =>{
                val (name, namePos) = constructName(id)
                val qname = QualifiedName(None, name)
                val args = constructList(argsTree, constructExpr, hasComma = true)
                Call(qname, args).setPos(namePos)
              }
            }
          }
          case Node('IdExprOpt ::= List(), List()) =>
            val (name, pos) = constructName(id)
            Variable(name).setPos(pos)
        }
      }

      case Node('lvl10 ::= List('LiteralWithoutUnit), List(lit)) =>
        constructLiteral(lit)

      case Node('lvl10 ::= List('Parenthesized), List(parenth)) =>
        parenth match {
          case Node('Parenthesized ::= _, List(_, parentOpt)) => {
            parentOpt match {
              case Node('ParenOpt ::= List(RPAREN()), List(Leaf(rpt))) =>
                //Literal unit value
                UnitLiteral().setPos(rpt)
              case Node('ParenOpt ::= List('Expr, RPAREN()), List(expr, Leaf(rpt))) =>{
                constructExpr(expr).setPos(rpt)
              }
            }
          }
        }
    }
  }

  @Override
  override def constructPattern(pTree: NodeOrLeaf[Token]): NominalTreeModule.Pattern = {
    pTree match {
      case Node('Pattern ::= List(UNDERSCORE()), List(Leaf(ut))) =>
        WildcardPattern().setPos(ut)
      case Node('Pattern ::= List('LiteralWithoutUnit), List(lit)) =>
        val literal = constructLiteral(lit)
        LiteralPattern(literal).setPos(literal)
      case Node('Pattern ::= List(LPAREN(), RPAREN()), List(Leaf(lpt))) =>
        val literal = UnitLiteral().setPos(lpt)
        LiteralPattern(literal).setPos(literal)
      case Node('Pattern ::= List('Id, 'IdPatternOpt), List(id, idPatOpt)) =>
        idPatOpt match{
          case Node('IdPatternOpt ::= List(), List()) =>{
            val (name, pos) = constructName(id)
            IdPattern(name).setPos(pos)
          }
          case Node('IdPatternOpt ::= List('QNameOpt, LPAREN(), 'Patterns, RPAREN()), List(qnameopt, _, patternsTree, _)) => {
            qnameopt match {
              case Node('QNameOpt ::= (DOT() :: _), List(_, nameTree)) => {
                val (module, modPos) = constructName(id)
                val (name, namePos) = constructName(nameTree)
                val qname = QualifiedName(Some(module), name)
                val patterns = constructList(patternsTree, constructPattern, hasComma = true)
                CaseClassPattern(qname, patterns).setPos(modPos)
              }
              case Node('QNameOpt ::= List(), List()) => {
                val (name, namePos) = constructName(id)
                val qname = QualifiedName(None, name)
                val patterns = constructList(patternsTree, constructPattern, hasComma = true)
                CaseClassPattern(qname, patterns).setPos(namePos)
              }
            }
          }
        }


    }
  }





  

  // Important helper method:
  // Because LL1 grammar is not helpful in implementing left associativity,
  // we give you this method to reconstruct it.
  // This method takes the left operand of an operator (leftopd)
  // as well as the tree that corresponds to the operator plus the right operand (ptree)
  // It parses the right hand side and then reconstruct the operator expression
  // with correct associativity.
  // If ptree is empty, it means we have no more operators and the leftopd is returned.
  // Note: You may have to override constructOp also, depending on your implementation
  def constructOpExpr(leftopd: Expr, ptree: NodeOrLeaf[Token]): Expr = {
    ptree match {
      case Node(_, List()) => //epsilon rule of the nonterminals
        leftopd
      case Node(sym ::= _, List(op, rightNode))
        if Set('OrExpr, 'AndExpr, 'EqExpr, 'CompExpr, 'AddExpr, 'MultExpr) contains sym =>
        rightNode match {
          case Node(_, List(nextOpd, suf)) => // 'Expr? ::= Expr? ~ 'OpExpr,
            val nextAtom = constructExpr(nextOpd)
            constructOpExpr(constructOp(op)(leftopd, nextAtom).setPos(leftopd), suf) // captures left associativity
        }
    }
  }

}

