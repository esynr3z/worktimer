FROM ubuntu:24.04

# Install necessary system dependencies
RUN apt-get update && \
    apt-get install -y --no-install-recommends \
    guile-3.0 guile-3.0-dev \
    ca-certificates curl git nano openssh-client make bash-completion pipx \
    locales \
    && locale-gen en_US.UTF-8 \
    && apt-get clean \
    && rm -rf /var/lib/apt/lists/*

ENV LANG=en_US.UTF-8
ENV LC_ALL=en_US.UTF-8

# Ensure the devcontainer user exists
RUN if ! id -u ubuntu >/dev/null 2>&1; then useradd -m -s /bin/bash ubuntu; fi

# Ensure /workspaces exists and is writable
RUN mkdir -p /workspaces && chown ubuntu:ubuntu /workspaces && chmod 775 /workspaces

# Switch to non-root user
USER ubuntu

# Install OpenCode
RUN curl -fsSL https://opencode.ai/install | bash

# Install pre-commit and commitizen
RUN pipx install pre-commit==4.5.1 commitizen==4.12.1
