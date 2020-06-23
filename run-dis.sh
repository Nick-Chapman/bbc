#!/usr/bin/env bash
set -euo pipefail -v

stack run roms/Basic2.rom > dis/Basic2.dis
stack run roms/Os12.rom > dis/Os12.dis
