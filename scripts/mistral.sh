#!/bin/bash

if [ -z "$1" ]; then
    echo "Usage: $0 <model_file.gguf>"
    exit 1
fi

cd `dirname "$0"`

cd hacks
if [ ! -e blob.bin ]; then
    g++ ligguf.cpp -o ligguf || exit 1
    ./ligguf "$1" blob.bin 1> blob.cfg
fi

cd ../../demo
ed -s mistral.asm <<'EOF'
/^;\*\*\*GEN/+1,/^;\*\*\*END/-1 d
/^;\*\*\*GEN/ r ../scripts/hacks/blob.cfg
w
q
EOF

cd ..
./asm -i demo/mistral.asm -o mistral.bin -m 0x80000000 -c || exit 10
./eplayer mistral.bin 0x80000000 0x80000000 scripts/hacks/blob.bin
