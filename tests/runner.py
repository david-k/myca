import sys
import subprocess
from pathlib import Path


TESTS_TO_IGNORE = {
    "struct_recursive_def",
}


#===============================================================================
# Helpers
#===============================================================================
CC = "gcc"


def run_test(test_src_dir: Path, test_build_dir: Path, myca_bin: Path) -> bool:
    expected_compilation_error_filepath = test_src_dir / "expected_compilation_error.txt"
    expected_return_code_filepath = test_src_dir / "expected_return_code.txt"

    print("Running " + test_src_dir.name + "... ", end="")
    if expected_compilation_error_filepath.exists():
        result = subprocess.run(
            [myca_bin, test_src_dir / "main.myca", "-o", test_build_dir / "main.c"],
            capture_output = True,
            text = True
        )

        expected = expected_compilation_error_filepath.read_text().strip()
        actual = result.stderr.strip()

        if actual == expected:
            print("ok")
            return True
        else:
            print("ERROR")
            print("Expected:")
            print(expected)
            print()
            print("Actual:")
            print(actual)
            print()
            return False

    elif expected_return_code_filepath.exists():
        compile_myca_result = subprocess.run(
            [myca_bin, test_src_dir / "main.myca", "-o", test_build_dir / "main.c"],
            capture_output = True,
            text = True
        )

        if compile_myca_result.returncode != 0:
            print(" compilation error")
            print(compile_myca_result.stderr)
            return False

        compile_c_result = subprocess.run(
            [CC, "-fsanitize=undefined", "-fsanitize=address", test_build_dir / "main.c", "-o", test_build_dir / "main"],
            capture_output = True,
            text = True
        )

        if compile_c_result.returncode != 0:
            print(" compilation error")
            print(compile_c_result.stderr)
            return False

        run_result = subprocess.run(
            [test_build_dir / "main"],
            capture_output = True,
            text = True
        )

        expected = int(expected_return_code_filepath.read_text().strip())
        actual = run_result.returncode

        if actual == expected:
            print("ok")
            return True
        else:
            print("ERROR")
            print(f"Expected return code: {expected}")
            print(f"Actual return code: {actual}")
            print()
            return False

    else:
        result = subprocess.run(
            [myca_bin, test_src_dir / "main.myca", "-o", test_build_dir / "main.c"],
            capture_output = True,
            text = True
        )

        if result.returncode == 0:
            print("ok")
            return True
        else:
            print("ERROR")
            print(result.stderr)
            print()
            return False


#===============================================================================
# Main
#===============================================================================
if len(sys.argv) != 3:
    print("USAGE: runner.py <PROJECT_DIR> <BUILD_DIR>")
    sys.exit(1)

project_dir = Path(sys.argv[1])
build_dir = Path(sys.argv[2])
myca_bin = build_dir / "myca"

if not myca_bin.exists():
    print("Build myca before running tests")
    sys.exit(1)

num_successes = 0
num_failures = 0
for test_src_dir in sorted((project_dir / "tests").iterdir()):
    if not test_src_dir.is_dir() or test_src_dir.name in TESTS_TO_IGNORE:
        continue

    test_build_dir = build_dir / "tests" / test_src_dir.name
    test_build_dir.mkdir(parents = True, exist_ok = True)
    if run_test(test_src_dir, test_build_dir, myca_bin):
        num_successes += 1
    else:
        num_failures += 1

print("------------------------------")
print(f"{num_successes} tests succeeded, {num_failures} failed, {len(TESTS_TO_IGNORE)} ignored")
print()

if num_failures > 0:
    sys.exit(1)
