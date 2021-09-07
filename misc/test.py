#!/usr/bin/python3

import subprocess
import os
import shutil
import argparse
import sys
import re
import pickle

from colr import color
from tabulate import tabulate

from collections import OrderedDict


PERF_MODE = False
IGNORE_LINE_WRAP = True
NEW_TEST = False
TEST_ALL = False

TEST_EXECUTABLE = 'build/test'
TEST_ASSEMBLY = 'build/test.s'

TEST_FOLDER = 'tests'
CC = "build/sally"
LINKER = "gcc"
LIBSYS = "misc/libsysy.a"
COMPILE_OPTIONS = ["-S", "-O2", '-o', TEST_ASSEMBLY]
LINKER_OPTIONS = [TEST_ASSEMBLY, LIBSYS, "-o", TEST_EXECUTABLE, '-static', '-march=armv7-a']

err = False

timing_table = OrderedDict()

def interpolate_from_green_to_red(t):
    if t<0:
        t = 0
    if t>1:
        t = 1
    # naive interpolating
    r = (int)(255 + (0-255)*t)
    g = (int)(0   + (255-0)*t)
    b = (int)(0)

    return (r, g, b)

def us2str(us, curr_us=-1):
    tm = '{:0.2f}s'.format(us/1e6)
    if curr_us != -1:
        percent = us/curr_us
        perc_str = ' ({:.0f}%)'.format(100*percent)
        colored_str = color(perc_str, fore=interpolate_from_green_to_red(percent))
        tm += colored_str

    return tm

def get_data(table, source, curr_us):
    try:
        return us2str(table[source], curr_us)
    except:
        return "No data"

def print_timing_table():
    tables = ['last.time', 'clang.time', 'clango3.time', 'gcc.time', 'gcco3.time', 'naive_sally.time']
    tables = ['misc/' + t for t in tables]
    tables = [pickle.load(open(t, 'rb')) for t in tables]

    curr_total_us = 0
    total_us = [0 for t in tables]

    table_data = []
    for source, curr_us in timing_table.items():
        curr_total_us += curr_us
        curr_tm = us2str(curr_us)
        row = [source, curr_tm]
        for i, t in enumerate(tables):
            try:
                total_us[i] += t[source]
                row.append(get_data(t, source, curr_us))
            except:
                row.append("No data")
                

        table_data.append(row)

    if TEST_ALL:
        row = ['Total', us2str(curr_total_us)]
        for us in total_us:
            row.append(us2str(us, curr_total_us))
        table_data.append(row)

    header = ['Test Case', 'Current: ' + CC, 'Last', 'clang -O2', 'clang -O3', 'gcc -O2', 'gcc -O3', 'Naive Sally']
    print(tabulate(table_data, headers=header))


def write_timing_table(table):
    pickle.dump(table, open('build/tm.time', 'wb'));

def extract_time(sysy_time_output):
    (h, m, s, us) = re.search('TOTAL: (\d+)H-(\d+)M-(\d+)S-(\d+)us', sysy_time_output).groups()
    return (int(h), int(m), int(s), int(us))

def read_file(filename):
    try:
        f = open(filename)
        return f.read()
    except IOError:
        return None

def write_file(filename, content):
    f = open(filename, 'w')
    f.write(content)
    f.close()

def run_command(input, *args):
    # @TODO: stderr?
    # @TODO: encode input to ascii?
    p = subprocess.run(args,
                        input=input,
                        stdout=subprocess.PIPE,
                        stderr=subprocess.PIPE)

    stdout = p.stdout.decode('utf-8')
    stderr = p.stderr.decode('utf-8')
    exit_code = p.returncode
    exit_code &= 0xff

    return (exit_code, stdout, stderr)

def run_test(source_file, input_file, output_file):
    LINK_ONLY = False
    if source_file.endswith('.s'):
        LINK_ONLY = True;
    if COMPILE_ONLY:
        print("Compiling", source_file)
    elif NEW_TEST:
        print("Compiling new test case using", CC, ':', source_file)
    else:
        print("Testing", source_file, end=' ', flush=True)
        expected_output = read_file(output_file)
        if expected_output is None:
            print("Output file not found: {}".format(output_file))
            return

    err = False

    input_text = read_file(input_file)

    if input_text is None:
        input_text = ''

    # compile the source file into assembly
    content = read_file(source_file)
    if content is None:
        print("[404 ERR] File not found:", source_file)
        err = True
        return

    if LINK_ONLY:
        write_file('build/test.s', content)
    else:

        if CC == 'gcc' or CC == 'clang' or CC == 'gcc-10.1':
            content = '#include "../misc/sylib.h"\n' + content

        write_file('build/test.c', content)
        (compiler_exit, compiler_output, compiler_err) = run_command('', CC, 'build/test.c', *COMPILE_OPTIONS)
        if NEW_TEST:
            print(compiler_output, end='')
            print(compiler_err, end='')
        if compiler_exit != 0:
            print("[ERR CE code {}]".format(compiler_exit))
            return

    # compile assembly to excutable
    (linker_exit, linker_output, linker_err) = run_command('', LINKER, *LINKER_OPTIONS)   
    if NEW_TEST:
        print(linker_output, end='')
        print(linker_err, end='')
    if linker_exit != 0:
        print(linker_output, end='')
        print(linker_err, end='')
        return

    if COMPILE_ONLY:
        return

    try:
        exit_code, output, output_timing = run_command(input_text.encode('ascii'), TEST_EXECUTABLE)
    except Exception as e:
        print(e)
        err = True
        print("[RUNTIME ERR]")
        return

    if PERF_MODE:
        try:
            (h, m, s, us) = extract_time(output_timing)
        except:
            print("[RUNTIME ERR]")
            err = True
            return
        s += m*60 + 3600*h

    output += str(exit_code)
    output += '\n'
    output = output.replace('\r', '')

    if NEW_TEST:
        write_file(output_file, output)
        print("Generated new test case to", output_file)
        print(output, end='')
        return


    if IGNORE_LINE_WRAP:
        output = output.replace('\n', '') + '\n'
        expected_output = expected_output.replace('\n', '') + '\n'

    if (output != expected_output):
        print("[ERR]")
        err = True
        if not PERF_MODE:
            print("\nExpected:\n----------\n{}----------".format(expected_output))
        print("Given\n----------\n{}----------\n".format(output), end='')
    else:
        print("[OK]", end='')
        if not PERF_MODE:
            print('')

    if (not err) and PERF_MODE:
        print("\tElapsed time: {}s-{}us".format(s, us))
        timing_table[source_file] = s*1000000 + us

def assemble_case(src):
    if src.endswith('.c'):
        last = 2
    elif src.endswith('.sy'):
        last = 3
    elif src.endswith('.s'):
        last = 2
    input_file  = src[:-last] + '.in'
    output_file = src[:-last] + '.out'
    return (src, input_file, output_file)
    

def get_test_sources(dir):
    test_files = []
    for file in os.listdir(dir):
        if file.endswith('.sy') or file.endswith('.c'):
            src = dir + '/' + file
            test_files.append(assemble_case(src))

    test_files.sort()
    return test_files

def run_tests(dir):
    test_cases = get_test_sources(dir)

    for case in test_cases:
        run_test(*case)


#exit_code, output = run_command("", "build\\sally.exe")
#print(output)

#print("exit code is", exit_code)

#s = input()
#run_test("tests/" + s)

parser = argparse.ArgumentParser(description="Run functional test")
parser.add_argument('-t', '--test_case', help="Specify a test case", dest='case')
parser.add_argument('-a', '--all', help="test all functional cases", action="store_true")
parser.add_argument('-p', '--perf', help="record the running time of each test case", action="store_true")
parser.add_argument('-c', '--compile', help="compile only", action="store_true")
parser.add_argument('-n', '--new_test', help="generator a new test case", action="store_true")
parser.add_argument('--CC', help="specify a compiler (default: build/sally)", type=str, default="build/sally")
parser.add_argument('-d', '--dir', help="specify folder of tests to run (default: tests)", type=str, default="tests")
args = parser.parse_args()

print("沙梨上班！")

TEST_FOLDER = args.dir
COMPILE_ONLY = args.compile
PERF_MODE = args.perf
NEW_TEST = args.new_test
CC = args.CC
if NEW_TEST:
    CC = 'gcc'

if CC == 'gcc' or CC == 'clang':
    LINKER = CC

if args.all:
    print("Testing all cases...")
    TEST_ALL = True
    run_tests(TEST_FOLDER)
    print("下班了！")
elif args.case is not None:
    run_test(*assemble_case(args.case))
else:
    err = True
    print("你想让沙梨干啥....")
    parser.print_help()

if PERF_MODE:
    write_timing_table(timing_table)
    print_timing_table()

sys.exit(int(err))
