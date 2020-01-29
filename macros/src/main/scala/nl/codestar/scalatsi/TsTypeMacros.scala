package nl.codestar.scalatsi

import com.avsystem.commons.macros.TypeClassDerivation

import scala.reflect.macros.blackbox

class TsTypeMacros(override val c: blackbox.Context) extends TypeClassDerivation {

  import c.universe._

  private val TypeObj: ModuleSymbol = c.prefix.actualType.termSymbol.asModule
  private val TypeCls: ClassSymbol  = TypeObj.companion.asClass

  final def ScalatsiPkg: Tree = q"_root_.nl.codestar.scalatsi"

  final def TSTypeObj: Tree = q"$ScalatsiPkg.TSType"
  final def TSTypeCls: Tree = tq"$ScalatsiPkg.TSType"

  override def implementDeferredInstance(tpe: Type): Tree =
    q"new $TypeObj.Deferred[$tpe]"

  override def typeClassInstance(tpe: Type): Type =
    getType(tq"$TypeCls[$tpe]")

  override def dependencyType(tpe: Type): Type =
    getType(tq"$TSTypeCls[$tpe]")

  override def dependency(depTpe: Type, tcTpe: Type, param: Symbol): Tree = {
    val clue     = s"Cannot materialize $tcTpe because of problem with parameter ${param.name}:\n"
    val depTcTpe = dependencyType(depTpe)
    Ident(inferCachedImplicit(depTcTpe, ErrorCtx(clue, param.pos)).name)
  }

  override def forSingleton(tpe: Type, singleValueTree: Tree): Tree =
    typecheckException("Singletons are not supported")

  override def forApplyUnapply(applyUnapply: ApplyUnapply, params: List[ApplyParam]): Tree = {
    val dTpe      = applyUnapply.ownerTpe.dealias
    val typeName  = TypeName("ProductTSIType")
    val nameBySym = targetNameMap(params.map(_.sym))

//    val members = params map { member =>
//      //      case (name, optional) if optional <:< typeOf[Option[_]] =>
//      //        val typeArg = optional.typeArgs.head
//      //        q"($name, ${getTSType(typeArg)} | TSUndefined)"
//      q"(${member.sym.name.decodedName.toString}, ${member.instance}.get)"
//    }

    q"""
     new $ScalatsiPkg.$typeName[$dTpe](
       ${dTpe.typeSymbol.name.decodedName.toString},
       ${mkArray(StringCls, params.map(p => nameBySym(p.sym)))}
     ) {
       def dependencies = {
         ..${cachedImplicitDeclarations(ci => q"val ${ci.name} = ${ci.body}")}
         ${mkArray(tq"$TSTypeCls[_]", params.map(_.instance))}
       }
     }
     """
  }

  override def forSealedHierarchy(tpe: Type, subtypes: List[KnownSubtype]): Tree = {
    val dTpe     = tpe.dealias
    val symbol   = dTpe.typeSymbol
    val name     = symbol.name.decodedName.toString
    val typeName = TypeName("SealedHierarchyTSNamedType")

    subtypes match {
      case Seq() =>
        c.warning(c.enclosingPosition, s"Sealed type has no known subclasses, could not generate union")
        mapToNever(tpe)
      case _ =>
        q"""
        new $ScalatsiPkg.$typeName[$dTpe](
          $name
        ) {
          def caseDependencies = {
            ..${cachedImplicitDeclarations(ci => q"val ${ci.name} = ${ci.body}")}
            ${mkArray(tq"$TSTypeCls[_]", subtypes.map(_.instance))}
          }
        }
        """
    }
  }

  override def forUnknown(tpe: Type): Tree =
    typecheckException("Unsupported type")

  def materializeRecursively[T: WeakTypeTag]: Tree = instrument {
    val tpe = weakTypeOf[T].dealias
    q"""
       implicit def ${c.freshName(TermName("allow"))}[T]: $AllowImplicitMacroCls[$TSTypeCls[T]] =
         $AllowImplicitMacroObj[$TSTypeCls[T]]
       $TypeObj.materialize[$tpe]
     """
  }

  private def mkArray[T: Liftable](elemTpe: Tree, elems: Seq[T]): Tree =
    q"""
      val res = new $ScalaPkg.Array[$elemTpe](${elems.size})
      ..${elems.zipWithIndex.map({ case (e, i) => q"res($i) = $e" })}
      res
     """

  private def targetNameMap(symbols: Seq[Symbol]): Map[Symbol, String] = {
    val paramsOrSubclasses =
      if (symbols.exists(_.isClass)) "Subclasses"
      else "Parameters or members"
    symbols.groupBy(st => st.name.decodedName.toString).map {
      case (targetName, List(sym)) => (sym, targetName)
      case (targetName, syms) =>
        abort(s"$paramsOrSubclasses ${syms.map(_.name).mkString(", ")} have the same @name: $targetName")
    }
  }

  private def mapToNever(tpe: Type): Tree =
    q"""{
       import _root_.nl.codestar.scalatsi.TSNamedType
       import _root_.nl.codestar.scalatsi.TypescriptType.{TSAlias, TSNever}
       TSNamedType(TSAlias(${tsName(tpe)}, TSNever))
     }"""

  private def tsName(tpe: Type): String = {
    val symbol = tpe.typeSymbol

    val prefix = Option(symbol)
      .collect({ case clsSymbol if clsSymbol.isClass => clsSymbol.asClass })
      .filterNot(_.isDerivedValueClass)
      .map(_ => "I")

    prefix.getOrElse("") + symbol.name.toString
  }
}
