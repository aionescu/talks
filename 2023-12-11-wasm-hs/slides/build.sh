#!/usr/bin/env bash
set -euo pipefail

marp --html true Slides.md "$@"
