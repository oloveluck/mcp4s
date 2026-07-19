#!/usr/bin/env python3
"""Extract ```scala blocks from docs/content into compilable test sources.

Run after editing documentation:  python3 scripts/snippet_harness.py
Then verify with:                 sbt examples/Test/compile
The generated sources under examples/src/test/scala/docsnippets/ are committed,
so CI's test compile fails if the published API drifts from the documentation.

All snippets in one page share a single object scope (docs snippets build on each
other), so later blocks see earlier definitions. Markers, placed as an HTML
comment on the line directly above a fence:
  <!-- doc-snippet: skip -->   never compiled (signature listings, pseudo-code)
  <!-- doc-snippet: reset -->  start a fresh scope (page redefines earlier names)
"""
import re
from pathlib import Path

ROOT = Path(__file__).resolve().parent.parent
DOCS = ROOT / "docs/content"
OUT = ROOT / "examples/src/test/scala/docsnippets"

FILE_HEADER = """\
// GENERATED from {md} — do not edit; regenerate with snippet_harness.py
package docsnippets.{pkg}

import cats.effect.{{IO, IOApp, Resource}}
import cats.syntax.all.*
import com.comcast.ip4s.*
import fs2.Stream
import io.circe.Json
import org.typelevel.otel4s.trace.Tracer
import mcp4s.protocol.*
import mcp4s.server.{{McpServer, Prompts, Resources, Server, ServiceRoutes, ToolContext, Tools}}
import mcp4s.server.transport.{{HttpConfig, SessionConfig, WebSocketConfig}}
import mcp4s.client.{{McpClient, McpClientBuilder, McpConnection}}
import mcp4s.client.transport.{{HttpTransportConfig, McpAuth, StdioTransportConfig, WebSocketTransportConfig}}
import mcp4s.transport.Timeouts

object stubs:
  def conn: McpConnection[IO]                  = ???
  def connection: McpConnection[IO]            = ???
  def httpClient: org.http4s.client.Client[IO] = ???
  def server: Server[IO]                       = ???
  def client: McpClient[IO]                    = ???
"""

def snippets_of(md: Path):
    lines = md.read_text().split("\n")
    out, i, directive = [], 0, None
    while i < len(lines):
        stripped = lines[i].strip()
        m = re.match(r"<!--\s*doc-snippet:\s*(\w+)\s*-->", stripped)
        if m:
            directive = m.group(1)
        elif stripped == "```scala":
            j = i + 1
            while j < len(lines) and lines[j].strip() != "```":
                j += 1
            out.append((directive or "compile", lines[i + 1 : j], i + 2))
            directive = None
            i = j
        elif stripped != "":
            directive = None
        i += 1
    return out

def main():
    OUT.mkdir(parents=True, exist_ok=True)
    for old in OUT.glob("*.scala"):
        old.unlink()
    total = skipped = 0
    for md in sorted(DOCS.rglob("*.md")):
        rel = md.relative_to(DOCS)
        snippets = snippets_of(md)
        if not snippets:
            continue
        pkg = re.sub(r"[^A-Za-z0-9]", "_", str(rel.with_suffix("")))
        parts = [FILE_HEADER.format(md=rel, pkg=pkg)]
        scope = 0
        open_scope = False
        for mode, code, lineno in snippets:
            total += 1
            if mode == "skip":
                skipped += 1
                continue
            if mode == "reset" or not open_scope:
                scope += 1
                parts.append(f"object scope_{scope}:")
                parts.append("  import stubs.{*, given}")
                open_scope = True
            parts.append(f"  // ---- snippet at line {lineno}")
            parts.extend("  " + c if c.strip() else "" for c in code)
            parts.append("")
        if open_scope:
            (OUT / f"{pkg}.scala").write_text("\n".join(parts) + "\n")
    print(f"snippets: {total}, skipped: {skipped}, files: {len(list(OUT.glob('*.scala')))}")

if __name__ == "__main__":
    main()
