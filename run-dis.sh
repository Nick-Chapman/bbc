#!/usr/bin/env bash
set -euo pipefail -v

stack run roms/Basic2.rom > out/Basic2.dis
stack run roms/Os12.rom > out/Os12.dis
