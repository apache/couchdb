#!/bin/sh

set -e

if [ ! -f ./.venv/bin/activate ]; then
    rm -rf ./.venv
    python3 -m venv .venv
    . ./.venv/bin/activate
    pip install --upgrade pip
    pip install -r requirements.txt
else
    . ./.venv/bin/activate
fi
