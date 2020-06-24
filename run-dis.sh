#!/usr/bin/env bash
set -euo pipefail -v

stack run -- --dis-basic > dis/Basic2.dis
stack run -- --dis-mos > dis/Os12.dis
