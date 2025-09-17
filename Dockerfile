# syntax=docker/dockerfile:1
FROM ubuntu:24.04

ARG DEBIAN_FRONTEND=noninteractive
RUN apt-get update && apt-get install -y --no-install-recommends \
    build-essential cmake ninja-build \
    llvm-17-dev clang-17 lld-17 ca-certificates git \
 && rm -rf /var/lib/apt/lists/*

ENV LLVM_DIR=/usr/lib/llvm-17/lib/cmake/llvm \
    CC=clang-17 \
    CXX=clang++-17

WORKDIR /work

# Dica: rode
#   docker build -t mycc-pt .
#   docker run --rm -it -v "$PWD":/work mycc-pt ./scripts/run_a3_all.sh

CMD ["/bin/bash", "-lc", "./scripts/run_a3_all.sh"]

