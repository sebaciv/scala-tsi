package nl.codestar
package scalatsi

import nl.codestar.scalatsi.TypescriptType._

object TypescriptTypeSerializer {
  // TODO: Optimize? Memoize? Tailrec?
  def serialize(tp: TypescriptType): String = tp match {
    case t: TypescriptNamedType => t.name
    case TSAny                  => "any"
    case TSArray(elements)      => serialize(elements) + "[]"
    case TSBoolean              => "boolean"
    case TSIndexedInterface(indexName, indexType, valueType) =>
      s"{ [ $indexName: ${serialize(indexType)} ]: ${serialize(valueType)} }"
    case TSIntersection(Seq())  => serialize(TSNever)
    case TSIntersection(Seq(e)) => serialize(e)
    case TSIntersection(of)     => s"${of.map(serialize) mkString " | "}"
    case TSNever                => "never"
    case TSNull                 => "null"
    case TSNumber               => "number"
    case TSObject               => "object"
    case TSTuple(members)       => s"[${members.map(serialize) mkString ", "}]"
    case TSString               => "string"
    case TSUndefined            => "undefined"
    case TSUnion(Seq())         => serialize(TSNever)
    case TSUnion(Seq(e))        => serialize(e)
    case TSUnion(of)            => s"(${of.map(serialize) mkString " | "})"
    case TSVoid                 => "void"
    case TSLiteralBoolean(v)    => v.toString()
    case TSLiteralNumber(v)     => v.toString()
    case TSLiteralString(v)     => s""""${v.replaceAllLiterally("\"", "\"\"")}""""
  }

  // Unfortunately no vararg generics in scala
  def emit[T](implicit tsType: TSNamedType[T]): String =
    emits(tsType)

  def emits(types: TSNamedType[_]*): String =
    discoverNestedNames(types).iterator
      .flatMap(emitNamed)
      .mkString("\n")

  private object TSInterfaceEntry {
    def unapply(typescriptType: TypescriptType): Option[(TypescriptType, Boolean)] =
      typescriptType match {
        case TSUnion(members) if members.contains(TSUndefined) =>
          Some((TSUnion(members.filter(_ != TSUndefined)), false))
        case other => Some((other, true))
      }
  }

  private def emitNamed(named: TypescriptNamedType): Opt[String] = named matchOpt {
    case TSAlias(name, underlying) =>
      s"export type $name = ${serialize(underlying)}"

    case TSEnum(name, const, entries) =>
      val mbs = entries.map({
        case (entryName, Some(i)) => s"  $entryName = $i"
        case (entryName, None)    => s"  $entryName"
      })
      s"""export ${if (const) "const " else ""}enum $name {
         |${mbs.mkString(",\n")}
         |}
       """.stripMargin

    case TSInterfaceIndexed(name, indexName, indexType, valueType) =>
      s"""export interface $name {
         |  [ $indexName: ${serialize(indexType)} ]: ${serialize(valueType)}
         |}
       """.stripMargin

    case TSInterface(name, members) =>
      def symbol(required: Boolean) = if (required) ":" else "?:"

      val mbs = members.map({
        case (memberName, TSInterfaceEntry(tp, required)) =>
          s"  $memberName${symbol(required)} ${serialize(tp)}"
      })

      s"""
         |export interface $name {
         |${mbs.mkString("\n")}
         |}
       """.stripMargin
  }

  private def discoverNestedNames(rootTypes: Seq[TSNamedType[_]]): Vector[TypescriptNamedType] = {
    val namesBuilder = MLinkedHashSet.empty[TypescriptNamedType]

    def rec(tpe: TSType[_]): Unit = {
      val descend = tpe.get match {
        case named: TypescriptNamedType => namesBuilder.add(named)
        case _                          => true
      }

      if (descend) {
        tpe match {
          case t: TSAggregateType => t.nested.foreach(rec)
          case _                  =>
        }
      }
    }

    rootTypes.foreach(rec)
    namesBuilder.toVector.reverse
  }

}
