# Multi-stage build for mcp4s Calculator Server

# Stage 1: Build with Mill
FROM eclipse-temurin:21-jdk AS builder

WORKDIR /app

# Install Mill launcher script
RUN curl -fLo /usr/local/bin/mill https://repo1.maven.org/maven2/com/lihaoyi/mill-dist/1.0.6/mill-dist-1.0.6-mill.sh && \
    chmod +x /usr/local/bin/mill

# Copy build files first for better caching
COPY build.mill .mill-version ./
COPY core/src core/src
COPY server/src server/src
COPY client/src client/src
COPY examples/src examples/src

# Build the examples assembly jar
RUN mill examples.assembly

# Stage 2: Runtime image
FROM eclipse-temurin:21-jre-alpine

WORKDIR /app

# Copy the assembled jar
COPY --from=builder /app/out/examples/assembly.dest/out.jar /app/calculator-server.jar

# Expose the MCP server port
EXPOSE 3000

# Run the calculator server
CMD ["java", "-cp", "/app/calculator-server.jar", "mcp4s.examples.CalculatorServer"]
