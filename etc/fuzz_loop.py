#!/bin/env python3

import subprocess
import os.path

counter = 0;
random_bytes = "/tmp/greenwasm_fuzz_random_bytes.bin"
random = "/tmp/greenwasm_fuzz_random.wasm"
test = "/tmp/greenwasm_fuzz_test.wasm"
work = "/tmp/greenwasm_fuzz_work.wasm"

def shell(s, *args):
    subprocess.run(s.format(*args), shell=True, check=True)

def fuzz_cmd(file):
    return "../target/debug/fuzz_launcher {}".format(file)

def testcase_filename(counter):
    return "testcases/testcase_{:03}".format(counter)

shell("mkdir -p testcases")

while True:
    shell("cat /dev/urandom | dd bs=1k count=10 > {}", random_bytes)
    shell("wasm-opt {} -ttf -O3 -o {}", random_bytes, random)

    try:
        shell(fuzz_cmd(random))
    except subprocess.CalledProcessError:
        shell("wasm-reduce {} '--command={}' -t {} -w {} -f", random, fuzz_cmd(test), test, work)

        filename = "{}.wasm".format(testcase_filename(counter))
        while os.path.isfile(filename):
            counter = counter + 1
            filename = "{}.wasm".format(testcase_filename(counter))

        unreduced = "{}.unreduced.wasm".format(testcase_filename(counter))

        shell("mv {} {}", work, filename)
        shell("mv {} {}", random, unreduced)
