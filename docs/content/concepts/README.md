# Concepts

These pages describe how mcp4s is put together and why. Read them to form a mental model; read
the code for the details.

- [Architecture](architecture.md): the layers, the request path on each side, and the design
  decisions that shape the API.
- [Schema](schema.md): one description of a type, interpreted as JSON Schema, codecs and prompt
  arguments.
- [Transports](transports.md): what stdio, Streamable HTTP and WebSocket each map the protocol
  onto, and how to choose.
- [Server-to-client traffic](server-to-client.md): sampling, elicitation, progress, logging,
  subscriptions and tasks, and how they reach the client on every transport.
