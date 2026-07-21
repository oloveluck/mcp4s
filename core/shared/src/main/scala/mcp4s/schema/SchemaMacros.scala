/*
 * Copyright 2025 MCP4S Contributors
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package mcp4s.schema

import scala.quoted.*

/** Compile-time extraction of metadata used by [[Schema]] derivation.
  *
  * All helpers here are quoted macros that run entirely at compile time, so they are safe on every
  * platform (JVM, JS, Native).
  */
object SchemaMacros:

  /** Extract field descriptions from `@description` annotations at compile time */
  inline def fieldDescriptions[A]: Map[String, String] = ${ fieldDescriptionsMacro[A] }

  /** Extract class-level `@description` annotation at compile time */
  inline def classDescription[A]: Option[String] = ${ classDescriptionMacro[A] }

  /** Get the simple class name at compile time */
  inline def typeName[A]: String = ${ typeNameMacro[A] }

  /** Extract constructor default values by field name at compile time.
    *
    * Only monomorphic classes are supported; for classes with type parameters this returns an empty
    * map (their defaults cannot be referenced without instantiating the type arguments).
    */
  inline def fieldDefaults[A]: Map[String, Any] = ${ fieldDefaultsMacro[A] }

  /** Derive a tool/prompt name from a class name. Strips common suffixes, converts PascalCase to
    * snake_case. Examples: "AddArgs" -> "add", "SmartCalcArgs" -> "smart_calc", "Add" -> "add"
    */
  def deriveName(className: String): String =
    val stripped = Seq("Args", "Input", "Params", "Request")
      .foldLeft(className) { (name, suffix) =>
        if name.endsWith(suffix) && name.length > suffix.length
        then name.dropRight(suffix.length)
        else name
      }
    stripped
      .replaceAll("([A-Z]+)([A-Z][a-z])", "$1_$2")
      .replaceAll("([a-z\\d])([A-Z])", "$1_$2")
      .toLowerCase

  private val DescriptionAnnotation = "mcp4s.protocol.description"

  private def fieldDescriptionsMacro[A: Type](using Quotes): Expr[Map[String, String]] =
    import quotes.reflect.*
    val tpe          = TypeRepr.of[A]
    val fields       = tpe.typeSymbol.primaryConstructor.paramSymss.flatten
    val descriptions = fields.flatMap { field =>
      field.annotations.collectFirst {
        case term @ Apply(_, List(Literal(StringConstant(desc))))
            if term.tpe.typeSymbol.fullName == DescriptionAnnotation =>
          field.name -> desc
      }
    }
    Expr(descriptions.toMap)

  private def classDescriptionMacro[A: Type](using Quotes): Expr[Option[String]] =
    import quotes.reflect.*
    val tpe  = TypeRepr.of[A]
    val desc = tpe.typeSymbol.annotations.collectFirst {
      case term @ Apply(_, List(Literal(StringConstant(desc))))
          if term.tpe.typeSymbol.fullName == DescriptionAnnotation =>
        desc
    }
    desc match
      case Some(d) => '{ Some(${ Expr(d) }) }
      case None    => '{ None }

  private def typeNameMacro[A: Type](using Quotes): Expr[String] =
    import quotes.reflect.*
    Expr(TypeRepr.of[A].typeSymbol.name)

  private def fieldDefaultsMacro[A: Type](using Quotes): Expr[Map[String, Any]] =
    import quotes.reflect.*
    val tpe = TypeRepr.of[A]
    val sym = tpe.typeSymbol
    if sym.typeMembers.exists(_.isTypeParam) || sym.primaryConstructor.paramSymss.exists(
        _.exists(_.isTypeParam)
      )
    then '{ Map.empty[String, Any] }
    else
      val companion = sym.companionModule
      val params    = sym.primaryConstructor.paramSymss.flatten.filterNot(_.isTypeParam)
      val entries: List[Expr[(String, Any)]] = params.zipWithIndex.flatMap { (param, idx) =>
        val defaultName = s"$$lessinit$$greater$$default$$${idx + 1}"
        companion.methodMember(defaultName).headOption.map { method =>
          val call = Ref(companion).select(method).asExprOf[Any]
          '{ (${ Expr(param.name) }, $call) }
        }
      }
      '{ Map(${ Varargs(entries) }*) }
