#!/bin/env python3

import subprocess
import os.path
import random

iter_counter = 0;
counter = 0;
random_bytes = "/tmp/greenwasm_fuzz_random_bytes.bin"
random_wasm = "/tmp/greenwasm_fuzz_random.wasm"
test = "/tmp/greenwasm_fuzz_test.wasm"
work = "/tmp/greenwasm_fuzz_work.wasm"

def shell(s, *args):
    subprocess.run(s.format(*args), shell=True, check=True)

def fuzz_launch_cmd(file):
    return "../target/debug/fuzz_launcher {}".format(file)

def fuzz_gen_cmd(random, out):
    return "../target/debug/fuzz_generator {} {}".format(random, out)

def testcase_filename(counter):
    return "testcases/testcase_{:03}".format(counter)

shell("mkdir -p testcases")
shell("cargo build")

while True:
    rcount = random.randint(0, 20) ** 2 + random.randint(0, 3)
    shell("cat /dev/urandom | dd bs=1k count={} > {} 2> /dev/null", rcount, random_bytes)
    shell(fuzz_gen_cmd(random_bytes, random_wasm))

    try:
        shell(fuzz_launch_cmd(random_wasm))
    except subprocess.CalledProcessError:
        shell("wasm-reduce {} '--command={}' -t {} -w {} -f", random_wasm, fuzz_launch_cmd(test), test, work)

        filename = "{}.wasm".format(testcase_filename(counter))
        while os.path.isfile(filename):
            counter = counter + 1
            filename = "{}.wasm".format(testcase_filename(counter))

        unreduced = "{}.unreduced.wasm".format(testcase_filename(counter))

        shell("mv {} {}", work, filename)
        shell("mv {} {}", random_wasm, unreduced)

    print("Iteration {} OK\n".format(iter_counter))
    iter_counter = iter_counter + 1
