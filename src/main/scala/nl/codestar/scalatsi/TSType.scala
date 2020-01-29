package nl.codestar.scalatsi

import com.avsystem.commons.derivation.{AllowImplicitMacro, DeferredInstance}
import nl.codestar.scalatsi.TypescriptType._

import scala.annotation.implicitNotFound
import scala.collection.immutable.ListMap

/* TODO: Move this somewhere to the docs
 * To define an implicit TSType[T]:
1. If the type maps directly to another type Other, use
    implicit val tsT: TSType[T] = TSType.Of[Other] //or
    implicit val tsT: TSType[T] = tsAlias[T, Other]
2. If T is a case class, use
    implicit val tsT: TSIType[T] = TSIType.fromCaseClass
3. Or use the DSL to build your own interface:
    import nl.codestar.scalatsi.dsl._
    implicit val tsT: TSIType[T] = tsInterface(
      "foo" -> classOf[String],
      "bar" -> classOf[Option[Int]]
    )
 */

@implicitNotFound("No TSType found for ${T}")
trait TSType[T] { self =>
  def get: TypescriptType
  override def equals(obj: scala.Any): Boolean = obj match {
    case o: TSType[_] => get == o.get
    case _            => false
  }
  override def hashCode(): Int  = get.hashCode()
  override def toString: String = s"TSType($get)"

  // Forwarders to the underlying TypescriptType
  def |(other: TypescriptType): TSUnion = get | other
  def |(other: TSType[_]): TSUnion      = this | other.get

  def asReference: TypescriptType = this match {
    case named: TSNamedType[_] => TSTypeReference(named.tpeName)
    case tpe                   => tpe.get
  }
}

object TSType extends RecursiveAutoTSType {
  private class TSTypeImpl[T](override val get: TypescriptType) extends TSType[T]

  final class Deferred[T] extends DeferredInstance[TSType[T]] with TSType[T] {
    override def get: TypescriptType = underlying.get

    override def hashCode(): Int = System.identityHashCode(underlying)
    override def equals(obj: Any): Boolean = obj match {
      case df: Deferred[_] => underlying eq df.underlying
      case _               => false
    }

    override def toString: String = s"TSDeferred"
  }

  def apply[T](tt: TypescriptType): TSType[T] = new TSTypeImpl(tt)

  def transformed[T](tt: TSType[_])(f: TypescriptType => TypescriptType): TSType[T] =
    new TSType[T] with TSAggregateType {
      override def get: TypescriptType = f(tt.asReference)

      override def nested: Seq[TSType[_]] = Seq(tt)
    }

  def transformed2[T](tt1: TSType[_], tt2: TSType[_])(f: (TypescriptType, TypescriptType) => TypescriptType): TSType[T] =
    new TSType[T] with TSAggregateType {
      override def get: TypescriptType = f(tt1.asReference, tt2.asReference)

      override def nested: Seq[TSType[_]] = Seq(tt1, tt2)
    }

  def transformed3[T](tt1: TSType[_], tt2: TSType[_], tt3: TSType[_])(
    f: (TypescriptType, TypescriptType, TypescriptType) => TypescriptType
  ): TSType[T] =
    new TSType[T] with TSAggregateType {
      override def get: TypescriptType = f(tt1.asReference, tt2.asReference, tt2.asReference)

      override def nested: Seq[TSType[_]] = Seq(tt1, tt2, tt3)
    }

  def materialize[T]: TSType[T] = macro TsTypeMacros.materialize[T]

  /** Get an implicit `TSType[T]` */
  def get[T](implicit tsType: TSType[T]): TSType[T] = tsType

  /** Get an implicit `TSType[T]` or generate a default one
    *
    * By default
    * Case class will use [[fromCaseClass]]
    * Sealed traits/classes will use [[fromSealed]]
    * */
  def getOrGenerate[T]: TSType[T] = macro Macros.getImplicitMappingOrGenerateDefault[T, TSType]

  /** Generate a typescript interface for a case class */
  def fromCaseClass[T]: TSIType[T] = macro Macros.generateInterfaceFromCaseClass[T]

  /** Generate a Typescript discriminated union from a scala sealed trait
    *
    * @example
    * ```
    * sealed trait AorB
    * case class A(foo: Int) extends AorB
    * case class B(bar: Int) extends AorB
    *
    * implicit val tsAorB = TSType.fromSealed[AorB]
    * ```
    *
    * wil produce
    *
    * `type AorB = A | B`
    * @see [Typescript docs on Discrimintated Unions](https://www.typescriptlang.org/docs/handbook/advanced-types.html#discriminated-unions)
    **/
  def fromSealed[T]: TSNamedType[T] = macro Macros.generateUnionFromSealedTrait[T]

  /** Uses the typescript type of Target whenever we're looking for the typescript type of Source
    * This will not generate a `type Source = Target` line like alias
    *
    * @see alias
    **/
  def sameAs[Source, Target](implicit tsType: TSType[Target]): TSType[Source] =
    TSType.transformed(tsType)(identity)

  /** Create a Typescript alias "T" for type T, with the definition of Alias
    *
    * @example alias[Foo, String] will generate typescript `type Foo = string`
    * @see sameAs
    */
  def alias[T, Alias](implicit tsType: TSType[Alias], ct: Manifest[T]): TSNamedType[T] =
    alias[T, Alias](ct.runtimeClass.getSimpleName)

  /** Create a Typescript alias "name" for type T, with the definition of Alias
    *
    * @example alias[Foo, String]("IFoo") will generate typescript `type IFoo = string`
    * @see sameAs
    */
  def alias[T, Alias](name: String)(implicit tsType: TSType[Alias]): TSNamedType[T] =
    new TSNamedType[T] {
      override def tpeName: String          = name
      override def get: TypescriptNamedType = TSAlias(tpeName, tsType.get)
    }

  /** Create a Typescript alias "name" for type T, with the definition of tsType
    *
    * @example alias[Foo]("IFoo", TSString) will generate typescript `type IFoo = string`
    * @see sameAs
    */
  def alias[T](name: String, tsType: TypescriptType): TSNamedType[T] =
    TSNamedType(TSAlias(name, tsType))

  /** Create "name" as the typescript type for T, with "name" being defined elsewhere
    * external[Foo]("IXyz") will use "IXyz" as the typescript type every time something contains a Foo
    */
  def external[T](name: String): TSNamedType[T] =
    TypescriptType.fromString(name) match {
      case t: TSTypeReference => TSNamedType(t)
      case t                  => throw new IllegalArgumentException(s"String $name is a predefined type $t")
    }

  /** Create an interface "name" for T
    *
    * @example interface[Foo]("MyFoo", "bar" -> TSString) will output "interface MyFoo { bar: string }" */
  def interface[T](name: String, members: (String, TypescriptType)*): TSIType[T] =
    TSIType(TSInterface(name, ListMap(members: _*)))

  /** Create an interface "IClassname" for T
    *
    * @example interface[Foo]("bar" -> TSString) will output "interface IFoo { bar: string }" */
  def interface[T](members: (String, TypescriptType)*)(implicit ct: Manifest[T]): TSIType[T] =
    interface[T]("I" + ct.runtimeClass.getSimpleName, members: _*)

  /** Create an indexed interface for T
    *
    * @example interfaceIndexed[Foo]("IFooLookup", "key", TSString, TSInt) will output "interface IFooLookup { [key: string] : Int }"
    */
  def interfaceIndexed[T](
    name: String,
    indexName: String = "key",
    indexType: TypescriptType = TSString,
    valueType: TypescriptType
  ): TSNamedType[T] =
    TSNamedType(TSInterfaceIndexed(name, indexName, indexType, valueType))
}

trait RecursiveAutoTSType { this: TSType.type =>

  /**
    * Like `materialize`, but descends into types that `T` is made of (e.g. case class field types).
    */
  def materializeRecursively[T]: TSType[T] =
    macro TsTypeMacros.materializeRecursively[T]

  /**
    * INTERNAL API. Should not be used directly.
    */
  implicit def materializeImplicitly[T](implicit allow: AllowImplicitMacro[TSType[T]]): TSType[T] =
    macro TsTypeMacros.materializeImplicitly[T]
}

@implicitNotFound(
  "Could not find an implicit TSNamedType[${T}] in scope. Make sure you created and imported a named typescript mapping for the type."
)
trait TSNamedType[T] extends TSType[T] { self =>
  def tpeName: String
  def get: TypescriptNamedType
  override def toString: String = s"TSNamedType($get)"
}

trait TSAggregateType {
  def nested: Seq[TSType[_]]
}

abstract class SealedHierarchyTSNamedType[T](
  typeRepr: String
) extends TSNamedType[T]
    with TSAggregateType {

  protected def caseDependencies: Array[TSType[_]]

  private[this] lazy val caseDeps = caseDependencies

  override def tpeName: String = typeRepr

  override lazy val get: TypescriptNamedType = {
    val types = caseDeps.iterator.map {
      case named: TSNamedType[_] => TSTypeReference(named.tpeName)
      case tpe                   => tpe.get
    }.toVector
    TSAlias(typeRepr, if (types.size == 1) types.head else TSUnion(types))
  }

  override def nested: Seq[TSType[_]] = caseDeps
}

object TSNamedType extends RecursiveAutoNamedTSType {
  private class TSNamedTypeImpl[T](override val get: TypescriptNamedType) extends TSNamedType[T] {
    override def tpeName: String = get.name
  }

  final class Deferred[T] extends DeferredInstance[TSNamedType[T]] with TSNamedType[T] {
    override def tpeName: String          = underlying.tpeName
    override def get: TypescriptNamedType = underlying.get

    override def hashCode(): Int = System.identityHashCode(underlying)
    override def equals(obj: Any): Boolean = obj match {
      case df: Deferred[_] => underlying eq df.underlying
      case _               => false
    }

    override def toString: String = s"TSNamedDeferred"
  }

  def apply[T](tt: TypescriptNamedType): TSNamedType[T] =
    new TSNamedTypeImpl(tt)

  /** Get an implicit `TSNamedType[T]` */
  def get[T](implicit tsType: TSNamedType[T]): TSNamedType[T] = tsType

  /** Get an implicit `TSNamedType[T]` or generate a default one
    *
    * @see [[TSType.getOrGenerate]]
    **/
  def getOrGenerate[T]: TSNamedType[T] = macro Macros.getImplicitMappingOrGenerateDefault[T, TSNamedType]

  def materialize[T]: TSNamedType[T] = macro TsTypeMacros.materialize[T]
}

trait RecursiveAutoNamedTSType { this: TSNamedType.type =>

  /**
    * Like `materialize`, but descends into types that `T` is made of (e.g. case class field types).
    */
  def materializeRecursively[T]: TSNamedType[T] =
    macro TsTypeMacros.materializeRecursively[T]

  /**
    * INTERNAL API. Should not be used directly.
    */
  implicit def materializeImplicitly[T](implicit allow: AllowImplicitMacro[TSNamedType[T]]): TSNamedType[T] =
    macro TsTypeMacros.materializeImplicitly[T]
}

@implicitNotFound(
  "Could not find an implicit TSIType[${T}] in scope. Make sure you created and imported a typescript interface for the type."
)
trait TSIType[T] extends TSNamedType[T] { self =>
  override def get: TSInterface
  override def toString: String = s"TSIType($get)"
}

abstract class ProductTSIType[T](
  name: String,
  fieldNames: Array[String]
) extends TSIType[T]
    with TSAggregateType {
  protected def dependencies: Array[TSType[_]]

  private[this] lazy val deps = dependencies

  override def tpeName: String = s"I$name"

  override lazy val get: TSInterface = {
    val fields = fieldNames.iterator.zipWithIndex.map {
      case (name, idx) =>
        name -> (deps(idx) match {
          case named: TSNamedType[_] => TSTypeReference(named.tpeName)
          case tpe                   => tpe.get
        })
    }.toSeq
    TSInterface(tpeName, ListMap(fields: _*))
  }

  override def nested: Seq[TSType[_]] = deps
}

object TSIType {
  private class TSITypeImpl[T](override val get: TSInterface) extends TSIType[T] {
    override def tpeName: String = get.name
  }
  def apply[T](tt: TSInterface): TSIType[T] = new TSITypeImpl(tt)

  /** Get an implicit TSIType[T] */
  def get[T](implicit tsType: TSIType[T]): TSIType[T] = tsType

  /** Get an implicit `TSIType[T]` or generate a default one
    *
    * @see [[TSType.getOrGenerate]]
    */
  def getOrGenerate[T]: TSIType[T] = macro Macros.getImplicitInterfaceMappingOrGenerateDefault[T, TSIType]
}
