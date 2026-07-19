ThisBuild / tlBaseVersion    := "0.2"
ThisBuild / organization     := "io.github.oloveluck"
ThisBuild / organizationName := "MCP4S Contributors"
ThisBuild / startYear        := Some(2025)
ThisBuild / licenses         := Seq(License.Apache2)
ThisBuild / developers       := List(tlGitHubDev("oloveluck", "MCP4S Contributors"))
ThisBuild / homepage         := Some(url("https://github.com/mcp4s/mcp4s"))
ThisBuild / scmInfo := Some(
  ScmInfo(url("https://github.com/mcp4s/mcp4s"), "scm:git:git@github.com:mcp4s/mcp4s.git")
)

val Scala3 = "3.8.4"
ThisBuild / scalaVersion               := Scala3
ThisBuild / crossScalaVersions         := Seq(Scala3)
ThisBuild / tlJdkRelease               := Some(17)
ThisBuild / githubWorkflowJavaVersions := Seq(JavaSpec.temurin("17"))

// Only publish from tags (v*). Don't auto-publish snapshots on `main` pushes — the
// Central Portal snapshot repo isn't enabled for this namespace (was 403'ing).
ThisBuild / tlCiReleaseBranches := Seq()

// Scala Native CI jobs need the LLVM/clang toolchain and GC libs.
ThisBuild / githubWorkflowBuildPreamble ++= Seq(
  WorkflowStep.Run(
    commands =
      List("sudo apt-get update", "sudo apt-get install -y clang libstdc++-12-dev libgc-dev"),
    name = Some("Install Scala Native dependencies"),
    cond = Some("matrix.project == 'rootNative'")
  )
)

// The dependency-graph submission job needs `contents: write`, which this repo's
// default GITHUB_TOKEN doesn't grant (403). Disable it rather than weaken permissions.
ThisBuild / tlCiDependencyGraphJob := false

// Run the MCP conformance suite as a dedicated CI job (manages the server itself).
// Pin JDK 17 — the build targets `tlJdkRelease := 17`, which cannot run on the
// default JDK 11 that added jobs would otherwise use.
ThisBuild / githubWorkflowAddedJobs += WorkflowJob(
  id = "conformance",
  name = "MCP Conformance",
  scalas = List(Scala3),
  javas = List(JavaSpec.temurin("17")),
  steps = githubWorkflowJobSetup.value.toList ++ List(
    WorkflowStep.Use(
      UseRef.Public("actions", "setup-node", "v4"),
      name = Some("Setup Node.js"),
      params = Map("node-version" -> "22")
    ),
    WorkflowStep.Sbt(List("conformance"), name = Some("Run MCP conformance suite"))
  )
)

// Pre-1.0 library — no binary-compatibility checks yet.
ThisBuild / tlMimaPreviousVersions := Set.empty

// === Dependency versions (newest stable available for JVM + JS + Scala Native 0.5) ===
// http4s and circe are intentionally held on their stable lines: their newer
// releases (http4s 1.0.0-Mxx, circe 0.15.0-Mx) are milestones with no Scala Native
// build, which would break the cross-build.
val CatsEffect      = "3.7.0"
val Circe           = "0.14.15"
val Fs2             = "3.13.0"
val Http4s          = "0.23.34"
val Otel4s          = "1.0.1"
val JdkHttpClient   = "0.9.2"
val ScodecBits      = "1.2.5"
val MunitCatsEffect = "2.2.0"
val MunitScalaCheck = "1.3.0"
val Laika           = "1.3.2"
val Weaver          = "0.10.1"
val HdrHistogram    = "2.2.2"

lazy val commonSettings = Seq(
  scalacOptions ++= Seq(
    "-Wunused:all",
    "-Wvalue-discard",
    "-Wnonunit-statement",
    "-Wshadow:all",
    "-Wimplausible-patterns"
  ),
  libraryDependencies ++= Seq(
    "org.typelevel" %%% "munit-cats-effect" % MunitCatsEffect % Test,
    "org.scalameta" %%% "munit-scalacheck"  % MunitScalaCheck % Test
  )
)

lazy val root = tlCrossRootProject.aggregate(core, server, client, testkit, examples, benchmarks)

lazy val core = crossProject(JVMPlatform, JSPlatform, NativePlatform)
  .crossType(CrossType.Full)
  .in(file("core"))
  .settings(name := "mcp4s-core")
  .settings(commonSettings)
  .settings(
    libraryDependencies ++= Seq(
      "org.typelevel" %%% "cats-effect"   % CatsEffect,
      "co.fs2"        %%% "fs2-core"      % Fs2,
      "io.circe"      %%% "circe-core"    % Circe,
      "io.circe"      %%% "circe-generic" % Circe,
      "io.circe"      %%% "circe-parser"  % Circe,
      "org.typelevel" %%% "otel4s-core"   % Otel4s,
      "org.scodec"    %%% "scodec-bits"   % ScodecBits
    )
  )

lazy val server = crossProject(JVMPlatform, JSPlatform, NativePlatform)
  .crossType(CrossType.Full)
  .in(file("server"))
  .dependsOn(core)
  .settings(name := "mcp4s-server")
  .settings(commonSettings)
  .settings(
    libraryDependencies ++= Seq(
      "org.typelevel" %%% "cats-effect"         % CatsEffect,
      "co.fs2"        %%% "fs2-core"            % Fs2,
      "co.fs2"        %%% "fs2-io"              % Fs2,
      "org.http4s"    %%% "http4s-ember-server" % Http4s,
      "org.http4s"    %%% "http4s-client"       % Http4s,
      "org.http4s"    %%% "http4s-dsl"          % Http4s,
      "org.http4s"    %%% "http4s-circe"        % Http4s,
      "org.typelevel" %%% "otel4s-core"         % Otel4s
    )
  )

lazy val client = crossProject(JVMPlatform, JSPlatform, NativePlatform)
  .crossType(CrossType.Full)
  .in(file("client"))
  .dependsOn(core)
  .settings(name := "mcp4s-client")
  .settings(commonSettings)
  .settings(
    libraryDependencies ++= Seq(
      "org.typelevel" %%% "cats-effect"         % CatsEffect,
      "co.fs2"        %%% "fs2-core"            % Fs2,
      "org.http4s"    %%% "http4s-ember-client" % Http4s,
      "org.http4s"    %%% "http4s-circe"        % Http4s,
      "org.typelevel" %%% "otel4s-core"         % Otel4s
    )
  )
  // The WebSocket client transport is JVM-only: http4s's JdkWSClient is built on
  // java.net.http (JDK 11+) and has no JS/Native build.
  .jvmSettings(
    libraryDependencies += "org.http4s" %% "http4s-jdk-http-client" % JdkHttpClient
  )

// Reusable, cross-platform test fixtures (configurable servers + deterministic
// clients) for exercising mcp4s servers/clients. Published so downstream users can
// test their own MCP servers, and reused by `examples` tests and `benchmarks`.
lazy val testkit = crossProject(JVMPlatform, JSPlatform, NativePlatform)
  .crossType(CrossType.Pure)
  .in(file("testkit"))
  .dependsOn(server, client)
  .settings(name := "mcp4s-testkit")
  .settings(commonSettings)
  // The weaver-based compliance/performance harness runs live ember servers and uses
  // HdrHistogram, so it is JVM-only (sources under testkit/.jvm/src). weaver-cats is a
  // compile-scope dep so the abstract suites can live in `main` for downstream users.
  .jvmSettings(
    libraryDependencies ++= Seq(
      "org.typelevel"    %% "weaver-cats"  % Weaver,
      "org.hdrhistogram"  % "HdrHistogram" % HdrHistogram
    ),
    testFrameworks += new TestFramework("weaver.framework.CatsEffect"),
    Test / parallelExecution := false
  )

// Examples are JVM-only: they run the ember server and provide a concrete
// OpenTelemetry backend (otel4s-oteljava).
lazy val examples = project
  .in(file("examples"))
  .enablePlugins(NoPublishPlugin)
  .dependsOn(server.jvm, client.jvm, testkit.jvm)
  .settings(commonSettings)
  .settings(
    name                                   := "mcp4s-examples",
    libraryDependencies += "org.typelevel" %% "otel4s-oteljava" % Otel4s,
    // Integration tests bind ember servers to ports; run them sequentially.
    Test / parallelExecution := false
  )

// JVM-only performance benchmarks: JMH microbenchmarks for the request hot path
// plus an end-to-end throughput/latency driver. Run with `benchmarks/Jmh/run`.
lazy val benchmarks = project
  .in(file("benchmarks"))
  .enablePlugins(NoPublishPlugin, JmhPlugin)
  .dependsOn(server.jvm, client.jvm, testkit.jvm)
  .settings(
    name := "mcp4s-benchmarks",
    // JMH-generated harness code and the benchmark bodies legitimately discard
    // values / use non-unit statements, so relax those lints for this module only.
    scalacOptions ~= (_.filterNot(
      Set("-Wnonunit-statement", "-Wvalue-discard", "-Wunused:all")
    )),
    libraryDependencies += "org.hdrhistogram" % "HdrHistogram" % "2.2.2"
  )

// Documentation site, generated from docs/content with Laika (JVM-only).
// `sbt docs/run` renders the site to docs/target/site.
lazy val docs = project
  .in(file("docs"))
  .enablePlugins(NoPublishPlugin)
  .settings(
    name                                   := "mcp4s-docs",
    libraryDependencies += "org.typelevel" %% "laika-io" % Laika
  )

// =============================================================================
// MCP conformance testing
// =============================================================================
// The official MCP conformance suite (https://github.com/modelcontextprotocol/
// conformance) lives in the `conformance/` git submodule and is driven via npx.
// These tasks manage the full lifecycle: start the examples ConformanceServer,
// wait for it to become healthy, run the suite, then shut the server down.
//
//   sbt conformance                          run the "active" suite on :3000
//   sbt "conformance --scenario tools-list"  run a single scenario
//   sbt "conformance --suite all"            run all scenarios (incl. pending)
//   sbt "conformance --url http://h:p/mcp"   target a different server URL
//   sbt conformanceList                      list available scenarios
// =============================================================================

lazy val conformance = inputKey[Unit](
  "Start the ConformanceServer, run the MCP conformance suite against it, then stop it."
)
lazy val conformanceList = taskKey[Unit]("List available MCP conformance scenarios.")

def conformanceDir(base: File): File = base / "conformance"

def ensureConformanceDeps(base: File, log: sbt.util.Logger): File = {
  val dir = conformanceDir(base)
  if (!(dir / "src" / "index.ts").exists()) {
    log.info("Initializing conformance git submodule...")
    sys.process.Process(Seq("git", "submodule", "update", "--init", "conformance"), base) ! log
  }
  if (!(dir / "node_modules").exists()) {
    log.info("Installing conformance npm dependencies (npm ci)...")
    if ((sys.process.Process(Seq("npm", "ci"), dir) ! log) != 0)
      sys.error("npm ci failed in conformance/")
  }
  dir
}

def waitForHealth(url: String, log: sbt.util.Logger, timeoutSeconds: Int): Unit = {
  import java.net.{HttpURLConnection, URI}
  def ok(): Boolean =
    try {
      val c = URI.create(url).toURL.openConnection().asInstanceOf[HttpURLConnection]
      c.setConnectTimeout(1000); c.setReadTimeout(1000); c.setRequestMethod("GET")
      val code = c.getResponseCode; c.disconnect(); code >= 200 && code < 300
    } catch { case _: Throwable => false }
  val deadline = System.currentTimeMillis() + timeoutSeconds * 1000L
  log.info(s"Waiting for server health at $url ...")
  while (!ok()) {
    if (System.currentTimeMillis() > deadline)
      sys.error(s"Server did not become healthy at $url within ${timeoutSeconds}s")
    Thread.sleep(1000)
  }
  log.info("Server is healthy.")
}

conformanceList := {
  val log = streams.value.log
  val dir = ensureConformanceDeps((ThisBuild / baseDirectory).value, log)
  if ((sys.process.Process(Seq("npx", "tsx", "src/index.ts", "list", "--server"), dir) ! log) != 0)
    sys.error("conformance list failed")
}

conformance := {
  val parsed = complete.DefaultParsers.spaceDelimited("<arg>").parsed
  val log    = streams.value.log
  val base   = (ThisBuild / baseDirectory).value
  val dir    = ensureConformanceDeps(base, log)

  // Parse CLI flags (mirrors the previous `mill conformance` interface).
  def flag(name: String): Option[String] = {
    val i = parsed.indexOf(name); if (i >= 0 && i + 1 < parsed.size) Some(parsed(i + 1)) else None
  }
  val url       = flag("--url").getOrElse("http://localhost:3000/mcp")
  val healthUrl = url.replaceAll("/mcp/?$", "") + "/health"
  val scenarioArgs = flag("--scenario")
    .map(s => Seq("--scenario", s))
    .getOrElse(Seq("--suite", flag("--suite").getOrElse("active")))
  val verboseArgs = if (parsed.contains("--verbose")) Seq("--verbose") else Seq.empty
  val baseline    = base / "conformance-baseline.yml"
  val baselineArgs =
    if (baseline.exists()) Seq("--expected-failures", baseline.getAbsolutePath) else Seq.empty
  val cmd = Seq("npx", "tsx", "src/index.ts", "server", "--url", url) ++
    scenarioArgs ++ verboseArgs ++ baselineArgs

  // Start the ConformanceServer as a background job; .value runs before the body.
  val service = bgJobService.value
  val handle  = (examples / Compile / bgRunMain).toTask(" mcp4s.examples.ConformanceServer").value
  try {
    waitForHealth(healthUrl, log, timeoutSeconds = 60)
    log.info(s"Running conformance: ${cmd.mkString(" ")}")
    if ((sys.process.Process(cmd, dir) ! log) != 0) sys.error("Conformance tests failed")
    log.info("Conformance tests passed.")
  } finally {
    log.info("Stopping ConformanceServer...")
    service.stop(handle)
  }
}
