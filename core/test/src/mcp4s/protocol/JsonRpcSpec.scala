package mcp4s.protocol

import io.circe.*
import io.circe.parser.*
import io.circe.syntax.*
import munit.FunSuite
import mcp4s.protocol.Codecs.given

class JsonRpcSpec extends FunSuite:

  // === RequestId Factory Tests ===

  test("RequestId.apply(String) creates StringId") {
    val id = RequestId("test-123")
    assertEquals(id, RequestId.StringId("test-123"))
  }

  test("RequestId.apply(Long) creates NumberId") {
    val id = RequestId(42L)
    assertEquals(id, RequestId.NumberId(42L))
  }

  // === JsonRpcError Factory Tests ===

  test("JsonRpcError.parseError creates error with correct code") {
    val err = JsonRpcError.parseError("Invalid JSON")
    assertEquals(err.code, JsonRpcErrorCode.ParseError)
    assertEquals(err.message, "Invalid JSON")
  }

  test("JsonRpcError.invalidRequest creates error with correct code") {
    val err = JsonRpcError.invalidRequest("Missing field")
    assertEquals(err.code, JsonRpcErrorCode.InvalidRequest)
  }

  test("JsonRpcError.methodNotFound creates error with correct code and message") {
    val err = JsonRpcError.methodNotFound("unknown/method")
    assertEquals(err.code, JsonRpcErrorCode.MethodNotFound)
    assertEquals(err.message, "Method not found: unknown/method")
  }

  test("JsonRpcError.invalidParams creates error with correct code") {
    val err = JsonRpcError.invalidParams("Wrong type")
    assertEquals(err.code, JsonRpcErrorCode.InvalidParams)
  }

  test("JsonRpcError.internalError creates error with correct code") {
    val err = JsonRpcError.internalError("Server crashed")
    assertEquals(err.code, JsonRpcErrorCode.InternalError)
  }

  // === JsonRpcRequest Tests ===

  test("JsonRpcRequest without params encodes correctly") {
    val req = JsonRpcRequest(RequestId.NumberId(1), "test/method", None)
    val json = req.asJson
    assertEquals(json.hcursor.get[String]("jsonrpc"), Right("2.0"))
    assertEquals(json.hcursor.get[Long]("id"), Right(1L))
    assertEquals(json.hcursor.get[String]("method"), Right("test/method"))
  }

  test("JsonRpcRequest with params encodes correctly") {
    val params = Json.obj("key" -> Json.fromString("value"))
    val req = JsonRpcRequest(RequestId.StringId("abc"), "my/method", Some(params))
    val json = req.asJson
    assertEquals(json.hcursor.get[String]("id"), Right("abc"))
    assert(json.hcursor.downField("params").succeeded)
  }

  test("JsonRpcRequest roundtrip") {
    val req = JsonRpcRequest(
      RequestId.NumberId(42),
      "tools/call",
      Some(Json.obj("name" -> Json.fromString("calculate")))
    )
    val json = req.asJson
    assertEquals(json.as[JsonRpcRequest], Right(req))
  }

  // === JsonRpcNotification Tests ===

  test("JsonRpcNotification without params encodes correctly") {
    val notif = JsonRpcNotification("notifications/initialized", None)
    val json = notif.asJson
    assertEquals(json.hcursor.get[String]("jsonrpc"), Right("2.0"))
    assertEquals(json.hcursor.get[String]("method"), Right("notifications/initialized"))
    // Should not have id field
    assert(json.hcursor.downField("id").failed)
  }

  test("JsonRpcNotification roundtrip") {
    val notif = JsonRpcNotification(
      "notifications/cancelled",
      Some(Json.obj("requestId" -> Json.fromLong(123)))
    )
    val json = notif.asJson
    assertEquals(json.as[JsonRpcNotification], Right(notif))
  }

  // === JsonRpcResponse Tests ===

  test("JsonRpcResponse encodes correctly") {
    val result = Json.obj("tools" -> Json.arr())
    val resp = JsonRpcResponse(RequestId.NumberId(1), result)
    val json = resp.asJson
    assertEquals(json.hcursor.get[String]("jsonrpc"), Right("2.0"))
    assertEquals(json.hcursor.get[Long]("id"), Right(1L))
    assert(json.hcursor.downField("result").succeeded)
    // Should not have error field
    assert(json.hcursor.downField("error").failed)
  }

  test("JsonRpcResponse roundtrip") {
    val resp = JsonRpcResponse(
      RequestId.StringId("test"),
      Json.obj("data" -> Json.fromString("result"))
    )
    val json = resp.asJson
    assertEquals(json.as[JsonRpcResponse], Right(resp))
  }

  // === JsonRpcErrorResponse Tests ===

  test("JsonRpcErrorResponse encodes correctly") {
    val err = JsonRpcError(-32601, "Method not found", Some(Json.fromString("details")))
    val resp = JsonRpcErrorResponse(RequestId.NumberId(1), err)
    val json = resp.asJson
    assertEquals(json.hcursor.get[String]("jsonrpc"), Right("2.0"))
    assertEquals(json.hcursor.downField("error").get[Int]("code"), Right(-32601))
    assertEquals(json.hcursor.downField("error").get[String]("message"), Right("Method not found"))
    // Should not have result field
    assert(json.hcursor.downField("result").failed)
  }

  test("JsonRpcErrorResponse roundtrip") {
    val resp = JsonRpcErrorResponse(
      RequestId.NumberId(5),
      JsonRpcError(-32600, "Invalid", None)
    )
    val json = resp.asJson
    assertEquals(json.as[JsonRpcErrorResponse], Right(resp))
  }

  // === JsonRpcMessage Polymorphic Tests ===

  test("JsonRpcMessage decodes request correctly") {
    val json = parse("""{"jsonrpc":"2.0","id":1,"method":"test"}""").toOption.get
    val msg = json.as[JsonRpcMessage]
    assert(msg.isRight)
    assert(msg.toOption.get.isInstanceOf[JsonRpcRequest])
  }

  test("JsonRpcMessage decodes notification correctly") {
    val json = parse("""{"jsonrpc":"2.0","method":"notifications/test"}""").toOption.get
    val msg = json.as[JsonRpcMessage]
    assert(msg.isRight)
    assert(msg.toOption.get.isInstanceOf[JsonRpcNotification])
  }

  test("JsonRpcMessage decodes response correctly") {
    val json = parse("""{"jsonrpc":"2.0","id":1,"result":{}}""").toOption.get
    val msg = json.as[JsonRpcMessage]
    assert(msg.isRight)
    assert(msg.toOption.get.isInstanceOf[JsonRpcResponse])
  }

  test("JsonRpcMessage decodes error response correctly") {
    val json = parse("""{"jsonrpc":"2.0","id":1,"error":{"code":-32600,"message":"Invalid"}}""").toOption.get
    val msg = json.as[JsonRpcMessage]
    assert(msg.isRight)
    assert(msg.toOption.get.isInstanceOf[JsonRpcErrorResponse])
  }

  test("JsonRpcMessage rejects invalid structure") {
    // Has both result and error - invalid
    val json = parse("""{"jsonrpc":"2.0","id":1}""").toOption.get
    val msg = json.as[JsonRpcMessage]
    assert(msg.isLeft)
  }

  // === Edge Cases ===

  test("Request with null id decodes correctly") {
    val json = parse("""{"jsonrpc":"2.0","id":null,"method":"test"}""").toOption.get
    val msg = json.as[JsonRpcRequest]
    assert(msg.isRight)
    assertEquals(msg.toOption.get.id, RequestId.NullId)
  }

  test("JsonRpcError with data field roundtrip") {
    val err = JsonRpcError(-32000, "Custom error", Some(Json.obj("details" -> Json.fromString("more info"))))
    val json = err.asJson
    assertEquals(json.as[JsonRpcError], Right(err))
  }
