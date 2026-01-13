package mcp4s.protocol

import munit.FunSuite

class McpErrorSpec extends FunSuite:

  // === McpError is Exception ===

  test("McpError is an Exception") {
    val err: Exception = McpError.ToolNotFound("test")
    assertEquals(err.getMessage, "Tool not found: test")
  }

  // === toJsonRpcError Conversion Tests ===

  test("ToolNotFound converts to MethodNotFound error code") {
    val mcpErr = McpError.ToolNotFound("calculate")
    val rpcErr = McpError.toJsonRpcError(mcpErr)
    assertEquals(rpcErr.code, JsonRpcErrorCode.MethodNotFound)
  }

  test("ResourceNotFound converts to InvalidParams error code") {
    val mcpErr = McpError.ResourceNotFound("file:///test.txt")
    val rpcErr = McpError.toJsonRpcError(mcpErr)
    assertEquals(rpcErr.code, JsonRpcErrorCode.InvalidParams)
  }

  test("PromptNotFound converts to InvalidParams error code") {
    val mcpErr = McpError.PromptNotFound("greeting")
    val rpcErr = McpError.toJsonRpcError(mcpErr)
    assertEquals(rpcErr.code, JsonRpcErrorCode.InvalidParams)
  }

  test("InvalidToolArguments converts to InvalidParams error code") {
    val mcpErr = McpError.InvalidToolArguments("calc", "bad args")
    val rpcErr = McpError.toJsonRpcError(mcpErr)
    assertEquals(rpcErr.code, JsonRpcErrorCode.InvalidParams)
  }

  test("ProtocolVersionMismatch converts to InvalidRequest error code") {
    val mcpErr = McpError.ProtocolVersionMismatch("1.0", "2.0")
    val rpcErr = McpError.toJsonRpcError(mcpErr)
    assertEquals(rpcErr.code, JsonRpcErrorCode.InvalidRequest)
  }

  test("NotInitialized converts to InvalidRequest error code") {
    val mcpErr = McpError.NotInitialized()
    val rpcErr = McpError.toJsonRpcError(mcpErr)
    assertEquals(rpcErr.code, JsonRpcErrorCode.InvalidRequest)
  }

  test("AlreadyInitialized converts to InvalidRequest error code") {
    val mcpErr = McpError.AlreadyInitialized()
    val rpcErr = McpError.toJsonRpcError(mcpErr)
    assertEquals(rpcErr.code, JsonRpcErrorCode.InvalidRequest)
  }

  test("MethodNotFound converts to MethodNotFound error code") {
    val mcpErr = McpError.MethodNotFound("unknown")
    val rpcErr = McpError.toJsonRpcError(mcpErr)
    assertEquals(rpcErr.code, JsonRpcErrorCode.MethodNotFound)
  }

  test("MethodNotSupported converts to MethodNotFound error code") {
    val mcpErr = McpError.MethodNotSupported("unsupported")
    val rpcErr = McpError.toJsonRpcError(mcpErr)
    assertEquals(rpcErr.code, JsonRpcErrorCode.MethodNotFound)
  }

  test("InternalError converts to InternalError code") {
    val mcpErr = McpError.InternalError("crash")
    val rpcErr = McpError.toJsonRpcError(mcpErr)
    assertEquals(rpcErr.code, JsonRpcErrorCode.InternalError)
  }

  // === Error message preservation ===

  test("toJsonRpcError preserves error message") {
    val mcpErr = McpError.ToolNotFound("my-tool")
    val rpcErr = McpError.toJsonRpcError(mcpErr)
    assert(rpcErr.message.contains("Tool not found: my-tool"))
  }
