#!/bin/sh

set -e

DIR=$(cd "$(dirname "$0")" && pwd)

cd "$DIR/tree-sitter-borlang"
npx tree-sitter-cli generate --no-bindings

echo 'done'
