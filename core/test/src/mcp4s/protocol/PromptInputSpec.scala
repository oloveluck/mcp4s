package mcp4s.protocol

import munit.FunSuite

class PromptInputSpec extends FunSuite:

  // === Basic Derivation Tests ===

  case class SimplePromptArgs(name: String, message: String) derives PromptInput

  test("PromptInput derives for simple case class") {
    val pi = PromptInput[SimplePromptArgs]
    assertEquals(pi.arguments.length, 2)
  }

  test("PromptInput generates PromptArguments with correct names") {
    val pi = PromptInput[SimplePromptArgs]
    assertEquals(pi.arguments.map(_.name), List("name", "message"))
  }

  test("PromptInput marks all non-optional fields as required") {
    val pi = PromptInput[SimplePromptArgs]
    assert(pi.arguments.forall(_.required))
  }

  test("PromptInput decodes valid argument map") {
    val pi = PromptInput[SimplePromptArgs]
    val args = Map("name" -> "Alice", "message" -> "Hello")
    assertEquals(pi.decode(args), Right(SimplePromptArgs("Alice", "Hello")))
  }

  test("PromptInput decode fails for missing required argument") {
    val pi = PromptInput[SimplePromptArgs]
    val args = Map("name" -> "Alice") // missing message
    assert(pi.decode(args).isLeft)
    assert(pi.decode(args).left.exists(_.contains("message")))
  }

  // === Optional Field Tests ===

  case class OptionalPromptArgs(
      required: String,
      optional: Option[String]
  ) derives PromptInput

  test("PromptInput marks Option fields as not required") {
    val pi = PromptInput[OptionalPromptArgs]
    val requiredArg = pi.arguments.find(_.name == "required").get
    val optionalArg = pi.arguments.find(_.name == "optional").get
    assertEquals(requiredArg.required, true)
    assertEquals(optionalArg.required, false)
  }

  test("PromptInput decodes with optional field present") {
    val pi = PromptInput[OptionalPromptArgs]
    val args = Map("required" -> "value", "optional" -> "extra")
    assertEquals(pi.decode(args), Right(OptionalPromptArgs("value", Some("extra"))))
  }

  test("PromptInput decodes with optional field absent") {
    val pi = PromptInput[OptionalPromptArgs]
    val args = Map("required" -> "value")
    assertEquals(pi.decode(args), Right(OptionalPromptArgs("value", None)))
  }

  // === Description Annotation Tests ===

  case class DescribedPromptArgs(
      @description("Name to greet") name: String,
      @description("Greeting style") style: Option[String]
  ) derives PromptInput

  test("PromptInput extracts @description annotations") {
    val pi = PromptInput[DescribedPromptArgs]
    val nameArg = pi.arguments.find(_.name == "name").get
    val styleArg = pi.arguments.find(_.name == "style").get
    assertEquals(nameArg.description, Some("Name to greet"))
    assertEquals(styleArg.description, Some("Greeting style"))
  }

  case class PartiallyDescribedPromptArgs(
      @description("Has description") described: String,
      undescribed: String
  ) derives PromptInput

  test("PromptInput handles partial @description annotations") {
    val pi = PromptInput[PartiallyDescribedPromptArgs]
    val describedArg = pi.arguments.find(_.name == "described").get
    val undescribedArg = pi.arguments.find(_.name == "undescribed").get
    assertEquals(describedArg.description, Some("Has description"))
    assertEquals(undescribedArg.description, None)
  }

  // === Calculator Example Pattern ===

  case class CalculatePromptArgs(
      @description("The operation: add, subtract, multiply, or divide") operation: String,
      @description("First number") a: String,
      @description("Second number") b: String
  ) derives PromptInput

  test("Calculator prompt args has correct arguments") {
    val pi = PromptInput[CalculatePromptArgs]
    assertEquals(pi.arguments.length, 3)
    assertEquals(pi.arguments.map(_.name), List("operation", "a", "b"))
    assert(pi.arguments.forall(_.required))
  }

  test("Calculator prompt args has descriptions") {
    val pi = PromptInput[CalculatePromptArgs]
    val opArg = pi.arguments.find(_.name == "operation").get
    assertEquals(opArg.description, Some("The operation: add, subtract, multiply, or divide"))
  }

  test("Calculator prompt args decodes correctly") {
    val pi = PromptInput[CalculatePromptArgs]
    val args = Map("operation" -> "add", "a" -> "1", "b" -> "2")
    assertEquals(pi.decode(args), Right(CalculatePromptArgs("add", "1", "2")))
  }

  // === Empty Map Tests ===

  test("PromptInput decode fails with empty map for required fields") {
    val pi = PromptInput[SimplePromptArgs]
    val result = pi.decode(Map.empty)
    assert(result.isLeft)
  }

  case class AllOptionalArgs(
      a: Option[String],
      b: Option[String]
  ) derives PromptInput

  test("PromptInput decodes empty map when all fields optional") {
    val pi = PromptInput[AllOptionalArgs]
    assertEquals(pi.decode(Map.empty), Right(AllOptionalArgs(None, None)))
  }

  // === Edge Cases ===

  case class SingleFieldArgs(value: String) derives PromptInput

  test("PromptInput works with single field") {
    val pi = PromptInput[SingleFieldArgs]
    assertEquals(pi.arguments.length, 1)
    assertEquals(pi.decode(Map("value" -> "test")), Right(SingleFieldArgs("test")))
  }

  case class ManyFieldsArgs(
      @description("Field 1") f1: String,
      @description("Field 2") f2: String,
      @description("Field 3") f3: Option[String],
      @description("Field 4") f4: String,
      @description("Field 5") f5: Option[String]
  ) derives PromptInput

  test("PromptInput handles many fields correctly") {
    val pi = PromptInput[ManyFieldsArgs]
    assertEquals(pi.arguments.length, 5)
    assertEquals(pi.arguments.count(_.required), 3) // f1, f2, f4
    assertEquals(pi.arguments.count(!_.required), 2) // f3, f5
  }
