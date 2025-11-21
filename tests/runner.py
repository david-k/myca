import sys
import subprocess
from pathlib import Path
from enum import Enum
from dataclasses import dataclass


TESTS_TO_IGNORE = {
    "struct_recursive_def",
}


#===============================================================================
# Helpers
#===============================================================================
CC = "gcc"

class Outcome(Enum):
    SUCCESS = 1
    FAILURE = 2

@dataclass
class MycaStep:
    expected_outcome: Outcome
    expected_output: Path|None

@dataclass
class RunStep:
    expected_return_code: int


class TestExecutionPlan:
    myca_step: MycaStep|None = None
    run_step: RunStep|None = None

    def expect_compilation_success(self, parser_output: Path|None = None):
        if self.myca_step is None:
            self.myca_step = MycaStep(
                expected_outcome = Outcome.SUCCESS,
                expected_output = parser_output,
            )
        else:
            if self.myca_step.expected_outcome != Outcome.SUCCESS:
                raise Exception("Invalid test execution plan")

            if parser_output is not None:
                if self.myca_step.expected_output is not None:
                    raise Exception("Invalid test execution plan")

                self.myca_step.expected_output = parser_output


    def expect_compilation_failure(self, error_output: Path):
        if self.run_step is not None:
            raise Exception("Invalid test execution plan")

        if self.myca_step is None:
            self.myca_step = MycaStep(
                expected_outcome = Outcome.FAILURE,
                expected_output = error_output,
            )
        else:
            if self.myca_step.expected_outcome != Outcome.FAILURE:
                raise Exception("Invalid test execution plan")

            if self.myca_step.expected_output is not None:
                raise Exception("Invalid test execution plan")

            self.myca_step.expected_output = error_output


    def expect_return_code(self, return_code: int):
        self.expect_compilation_success()

        if self.run_step is not None:
            raise Exception("Invalid test execution plan")

        self.run_step = RunStep(expected_return_code = return_code)


    def has_steps(self) -> bool:
        return self.myca_step is not None or self.run_step is not None


    def execute(self, test_src_dir: Path, test_build_dir: Path) -> bool:
        if self.myca_step is not None:
            stdout_filename = test_build_dir / "stdout.txt"
            stderr_filename = test_build_dir / "stderr.txt"
            return_code = self._run_and_save_output(
                [
                    myca_bin,
                    test_src_dir / "main.myca",
                    "-o", test_build_dir / "main.c",
                    "--print-types",
                    "--enable-log", test_build_dir / "compiler_log.html",
                ],
                stdout_filename,
                stderr_filename,
            )

            if return_code == 0:
                if not self._handle_myca_compilation_success(stdout_filename):
                    return False
            else:
                if not self._handle_myca_compilation_failure(stderr_filename):
                    return False

        if self.run_step is not None:
            cc_result = subprocess.run(
                [CC, "-fsanitize=undefined", "-fsanitize=address", test_build_dir / "main.c", "-o", test_build_dir / "main"],
                capture_output = True,
                text = True
            )

            if cc_result.returncode != 0:
                print("ERROR")
                print("C compilation error")
                print(cc_result.stderr)
                print()
                return False

            run_result = subprocess.run(
                [test_build_dir / "main"],
                capture_output = True,
                text = True
            )

            actual = run_result.returncode
            if actual != self.run_step.expected_return_code:
                print("ERROR")
                print(f"Expected return code: {self.run_step.expected_return_code}")
                print(f"Actual return code: {actual}")
                print()
                return False

        print("ok")
        return True


    def _handle_myca_compilation_failure(self, stderr_filename: Path) -> bool:
        assert self.myca_step is not None

        actual_output = stderr_filename.read_text().strip()
        if self.myca_step.expected_outcome == Outcome.SUCCESS:
            print("ERROR")
            print("Compilation failed")
            print(actual_output)
            print()
            return False
        else:
            if self.myca_step.expected_output is not None:
                expected_output = self.myca_step.expected_output.read_text().strip()
                if expected_output != actual_output:
                    print("ERROR")
                    self._show_diff(self.myca_step.expected_output, stderr_filename)
                    print()
                    return False

        return True


    def _handle_myca_compilation_success(self, stdout_filename: Path) -> bool:
        assert self.myca_step is not None

        actual_output = stdout_filename.read_text().strip()
        if self.myca_step.expected_outcome == Outcome.SUCCESS:
            if self.myca_step.expected_output is not None:
                expected_output = self.myca_step.expected_output.read_text().strip()
                if expected_output != actual_output:
                    print("ERROR")
                    self._show_diff(self.myca_step.expected_output, stdout_filename)
                    print()
                    return False
        else:
            print("ERROR")
            print("Compilation should have failed but succeeded")
            print()
            return False

        return True


    def _run_and_save_output(self, cmd: list, stdout_filename: Path, stderr_filename: Path) -> int:
        with (
            open(stdout_filename, "w+") as stdout_file,
            open(stderr_filename, "w+") as stderr_file
        ):
            result = subprocess.run(cmd, stdout = stdout_file, stderr = stderr_file)
            return result.returncode


    def _show_diff(self, expected_output: Path, actual_output: Path):
        subprocess.run([
            "diff", "--color", "-C", "1",
            "--label", "expected", expected_output,
            "--label", "actual", actual_output
        ])


def run_test(test_src_dir: Path, test_build_dir: Path, myca_bin: Path) -> bool:
    expected_parser_output_filepath = test_src_dir / "expected_parser_output.txt"
    expected_compilation_error_filepath = test_src_dir / "expected_compilation_error.txt"
    expected_return_code_filepath = test_src_dir / "expected_return_code.txt"

    plan = TestExecutionPlan()
    if expected_parser_output_filepath.exists():
        plan.expect_compilation_success(expected_parser_output_filepath)

    if expected_compilation_error_filepath.exists():
        plan.expect_compilation_failure(expected_compilation_error_filepath)

    if expected_return_code_filepath.exists():
        expected_return_code = int(expected_return_code_filepath.read_text().strip())
        plan.expect_return_code(expected_return_code)

    print("Running " + test_src_dir.name + "... ", end="")

    if not plan.has_steps():
        print("ERROR: Invalid test")
        print("No execution steps defined")
        return False

    return plan.execute(test_src_dir, test_build_dir)


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
print(f"{num_successes} / {num_successes + num_failures} tests passed", end="")

if len(TESTS_TO_IGNORE):
    print(f" ({len(TESTS_TO_IGNORE)} ignored)", end="")

print("\n")

if num_failures > 0:
    sys.exit(1)
