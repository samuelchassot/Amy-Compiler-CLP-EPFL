package amyc
package analyzer

import utils._
import ast.{Identifier, NominalTreeModule => N, SymbolicTreeModule => S}

// Name analyzer for Amy
// Takes a nominal program (names are plain strings, qualified names are string pairs)
// and returns a symbolic program, where all names have been resolved to unique Identifiers.
// Rejects programs that violate the Amy naming rules.
// Also populates and returns the symbol table.
object NameAnalyzer extends Pipeline[N.Program, (S.Program, SymbolTable)] {
  def run(ctx: Context)(p: N.Program): (S.Program, SymbolTable) = {
    import ctx.reporter._

    // Step 0: Initialize symbol table
    val table = new SymbolTable

    // Step 1: Add modules to table 
    val modNames = p.modules.groupBy(_.name)
    modNames.foreach { case (name, modules) =>
      if (modules.size > 1) {
        fatal(s"Two modules named $name in program", modules.head.position)
      }
    }

    modNames.keys.toList foreach table.addModule


    // Helper method: will transform a nominal type 'tt' to a symbolic type,
    // given that we are within module 'inModule'.
    def transformType(tt: N.TypeTree, inModule: String): S.Type = {
      tt.tpe match {
        case N.IntType => S.IntType
        case N.BooleanType => S.BooleanType
        case N.StringType => S.StringType
        case N.UnitType => S.UnitType
        case N.ClassType(qn@N.QualifiedName(module, name)) =>
          table.getType(module getOrElse inModule, name) match {
            case Some(symbol) =>
              S.ClassType(symbol)
            case None =>
              fatal(s"Could not find type $qn", tt)
          }
      }
    }

    // Step 2: Check name uniqueness of definitions in each module
    p.modules.foreach({
      m => m.defs.groupBy(_.name).foreach({case (name, definits) =>
                                            if(definits.size > 1){
                                                fatal(s"two definitions named $name in module", definits.head.position)
      }})
    })

    // Step 3: Discover types and add them to symbol table
    p.modules.foreach(m => m.defs.foreach(
      d => d match{
        case N.AbstractClassDef(name) => table.addType(m.name, name)
        case _ =>
      }
    ))

    // Step 4: Discover type constructors, add them to table
    p.modules.foreach(m => m.defs.foreach(
      d => d match{
        case N.CaseClassDef(name, fields, parnt) => {
          val newFields = fields.map(transformType(_, m.name))
          val newParent = table.getType(m.name, parnt)
          newParent match{
            case None => fatal(s"the type $parnt that $name extends does not exist in module $m", d.position)
            case Some(parntId) =>table.addConstructor(m.name, name, newFields, parntId)
          }

        }
        case _ =>
      }
    ))

    // Step 5: Discover functions signatures, add them to table
    p.modules.foreach(m => m.defs.foreach(
      d => d match{
        case N.FunDef(name, parms, rType, bdy) => {
          val newParms = parms.map(
            prmDef => transformType(prmDef.tt, m.name))
          table.addFunction(m.name, d.name, newParms, transformType(rType, m.name))
        }
        case _ =>
      }
    ))

    // Step 6: We now know all definitions in the program.
    //         Reconstruct modules and analyse function bodies/ expressions
    
    // This part is split into three transfrom functions,
    // for definitions, FunDefs, and expressions.
    // Keep in mind that we transform constructs of the NominalTreeModule 'N' to respective constructs of the SymbolicTreeModule 'S'.
    // transformFunDef is given as an example, as well as some code for the other ones

    def transformDef(df: N.ClassOrFunDef, module: String): S.ClassOrFunDef = { df match {
      case N.AbstractClassDef(name) => {
        val id = table.getType(module, df.name)
        id match {
          case Some(nameID) => S.AbstractClassDef(nameID).setPos(df.position)
          case None => fatal(s"Type $name not found in module $module", df.position)
        }
      }
      case N.CaseClassDef(name, _, _) =>
        val constr = table.getConstructor(module, name)
        constr match{
          case None => fatal(s"Case class $name not found in module $module", df.position)
          case Some((id, constrSig)) =>{
            val typesTrees = constrSig.argTypes.map(t => S.TypeTree(t))
            S.CaseClassDef(id, typesTrees, constrSig.parent)
          }
        }


      case fd: N.FunDef =>
        transformFunDef(fd, module)
    }}.setPos(df)

    def transformFunDef(fd: N.FunDef, module: String): S.FunDef = {
      val N.FunDef(name, params, retType, body) = fd
      val Some((sym, sig)) = table.getFunction(module, name)

      params.groupBy(_.name).foreach { case (name, ps) =>
        if (ps.size > 1) {
          fatal(s"Two parameters named $name in function ${fd.name}", fd)
        }
      }

      val paramNames = params.map(_.name)

      val newParams = params zip sig.argTypes map { case (pd@N.ParamDef(name, tt), tpe) =>
        val s = Identifier.fresh(name)
        S.ParamDef(s, S.TypeTree(tpe).setPos(tt)).setPos(pd)
      }

      val paramsMap = paramNames.zip(newParams.map(_.name)).toMap

      S.FunDef(
        sym,
        newParams,
        S.TypeTree(sig.retType).setPos(retType),
        transformExpr(body)(module, (paramsMap, Map()))
      ).setPos(fd)
    }

    // This function takes as implicit a pair of two maps:
    // The first is a map from names of parameters to their unique identifiers,
    // the second is similar for local variables.
    // Make sure to update them correctly if needed given the scoping rules of Amy
    def transformExpr(expr: N.Expr)
                     (implicit module: String, names: (Map[String, Identifier], Map[String, Identifier])): S.Expr = {
      val (params, locals) = names
      val res = expr match {
        case N.Match(scrut, cases) =>
          // Returns a transformed pattern along with all bindings
          // from strings to unique identifiers for names bound in the pattern.
          // Also, calls 'fatal' if a new name violates the Amy naming rules.
          def transformPattern(pat: N.Pattern): (S.Pattern, List[(String, Identifier)]) = {
            pat match{
              case N.WildcardPattern() => (S.WildcardPattern(), Nil)
              case N.IdPattern(name) => {
                val opt = table.getConstructor(module, name)
                if (opt.isEmpty) {
                  warning(s"Do you mean $name()", pat.position)
                }
                if (names._2.contains(name)) {
                  fatal(s"a variable with the name $name already exists in this scope", pat.position)
                }
                if (names._1.contains(name)) {
                  warning(s"The variable with the name $name will shadow the parameter with name $name", pat.position)
                }

                val id = Identifier.fresh(name)
                names._2 + (name, id)
                (S.IdPattern(id), List((name, id)))
              }

              case N.LiteralPattern(valueLit) => {
                valueLit match{
                  case N.IntLiteral(value) => (S.LiteralPattern(S.IntLiteral(value)).setPos(pat), Nil)
                  case N.BooleanLiteral(value) => (S.LiteralPattern(S.BooleanLiteral(value)).setPos(pat), Nil)
                  case N.StringLiteral(value) => (S.LiteralPattern(S.StringLiteral(value)).setPos(pat), Nil)
                  case N.UnitLiteral() => (S.LiteralPattern(S.UnitLiteral()).setPos(pat), Nil)
                }
              }

              case N.CaseClassPattern(constr, args) =>{
                val constrModule = if(constr.module.isEmpty) module else constr.module.get
                val opt = table.getConstructor(module, constr.name)
                if(opt.isEmpty){
                  fatal(s"Cannot find the Constructor $module.$name in the scope", pat.position)
                }
                def constructArgs(args : List[N.Pattern]) : List[(S.Pattern, List[(String, Identifier)])] = {
                  args match{
                    case arg :: tail => transformPattern(arg)::constructArgs(tail)
                    case Nil => Nil
                  }
                }
                val constructedArgs = constructedArgs(args)
                val sArgs = constructedArgs.map(_._1)
                val patterns = constructedArgs.map(_._2)
              }

            }

          }

          def transformCase(cse: N.MatchCase) = {
            val N.MatchCase(pat, rhs) = cse
            val (newPat, moreLocals) = transformPattern(pat)
            ???  // TODO
          }

          S.Match(transformExpr(scrut), cases.map(transformCase))

        case _ =>
          ???  // TODO: Implement the rest of the cases
      }
      res.setPos(expr)
    }

    // Putting it all together to construct the final program for step 6.
    val newProgram = S.Program(
      p.modules map { case mod@N.ModuleDef(name, defs, optExpr) =>
        S.ModuleDef(
          table.getModule(name).get,
          defs map (transformDef(_, name)),
          optExpr map (transformExpr(_)(name, (Map(), Map())))
        ).setPos(mod)
      }
    ).setPos(p)

    (newProgram, table)

  }
}
