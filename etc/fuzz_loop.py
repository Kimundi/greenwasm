#!/bin/env python3

import subprocess
import os.path

def shell(s):
    subprocess.run(s, shell=True, check=True)

counter = 0;
shell("mkdir -p testcases")

while True:
    shell("cat /dev/urandom | dd bs=1k count=10 > /tmp/greenwasm_fuzz_random_bytes.bin")
    shell("wasm-opt /tmp/greenwasm_fuzz_random_bytes.bin -ttf -O3 -o /tmp/greenwasm_fuzz_random.wasm")

    try:
        shell("../target/debug/fuzz_launcher /tmp/greenwasm_fuzz_random.wasm")
    except subprocess.CalledProcessError:
        print("foo")
        shell("wasm-reduce /tmp/greenwasm_fuzz_random.wasm '--command=../target/debug/fuzz_launcher /tmp/greenwasm_fuzz_test.wasm' -t /tmp/greenwasm_fuzz_test.wasm -w /tmp/greenwasm_fuzz_work.wasm -f")

        filename = "testcases/testcase_" + str(counter) + ".wasm"
        while os.path.isfile(filename):
            counter = counter + 1
            filename = "testcases/testcase_" + str(counter) + ".wasm"

        shell("mv /tmp/greenwasm_fuzz_work.wasm " + filename)
