# Base image: Eclipse Temurin JDK 11 on Ubuntu Jammy (x86-64)
FROM --platform=linux/amd64 eclipse-temurin:11-jdk-jammy

# ── System dependencies ────────────────────────────────────────────────────────
RUN apt-get update && apt-get install -y --no-install-recommends \
    curl \
    gnupg \
    python3 \
    python3-pip \
    z3 \
    && rm -rf /var/lib/apt/lists/*

# ── SBT ───────────────────────────────────────────────────────────────────────
# Install SBT 1.6.2 to match project/build.properties
RUN curl -fsSL "https://github.com/sbt/sbt/releases/download/v1.6.2/sbt-1.6.2.tgz" \
    | tar -xz -C /usr/local \
    && ln -s /usr/local/sbt/bin/sbt /usr/local/bin/sbt

# ── solc 0.8.29 ───────────────────────────────────────────────────────────────
RUN curl -fsSL \
    "https://binaries.soliditylang.org/linux-amd64/solc-linux-amd64-v0.8.29+commit.ab55807c" \
    -o /usr/local/bin/solc \
    && chmod +x /usr/local/bin/solc

# ── Working directory ──────────────────────────────────────────────────────────
WORKDIR /app

# ── Copy project files ─────────────────────────────────────────────────────────
# Copy dependency-resolution files first so Docker can cache the ivy/coursier
# download layer independently of source changes.
COPY project/           project/
COPY build.sbt          build.sbt
COPY unmanaged/         unmanaged/

# Copy the bundled Z3 4.8.14 native libraries that the project depends on.
# libz3.so and libz3java.so must be on LD_LIBRARY_PATH at runtime.
COPY libz3.so           libz3.so
COPY libz3java.so       libz3java.so
COPY z3-4.8.14/         z3-4.8.14/

# Now copy the full source tree.
COPY src/               src/

# ── Native library path ────────────────────────────────────────────────────────
# Point the JVM's native-lib loader at the project root where libz3*.so live.
ENV LD_LIBRARY_PATH=/app

# ── Pre-build fat jar ──────────────────────────────────────────────────────────
RUN sbt assembly

# ── Node.js 18 + Claude Code ───────────────────────────────────────────────────
RUN curl -fsSL https://deb.nodesource.com/setup_18.x | bash - \
    && apt-get install -y --no-install-recommends nodejs \
    && rm -rf /var/lib/apt/lists/* \
    && npm install -g @anthropic-ai/claude-code

# ── Non-root user ──────────────────────────────────────────────────────────────
RUN useradd -m -s /bin/bash user \
    && chown -R user:user /app
USER user

# ── Default command ────────────────────────────────────────────────────────────
CMD ["bash"]
