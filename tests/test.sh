#!/usr/bin/env bash

readonly COMPILER_EXIT_CODE_FILE="compiler-exit-code.txt"
readonly EXIT_CODE_FILE="exit-code.txt"
readonly PROGRAM_STDOUT_FILE="stdout.txt"

get_compiler_exit_code() {
  local path=$1
  echo 
}

for test_path in tests/resources/*;
do
  compiler_exit_code=$(<"${test_path}/${COMPILER_EXIT_CODE_FILE}")
  exit_code=$(<"${test_path}/${EXIT_CODE_FILE}")
  program_stdout=$(<"${test_path}/${PROGRAM}")
  
  source_file_path="${test_path}/main.c"
  assembly_file_path="${test_path}/main.s"
  out_file_path="${test_path}/out"

  ./hcc $source_file_path 2>&1 /dev/null
  actual_compiler_exit_code=$?
  if [[ $actual_compiler_exit_code -ne $compiler_exit_code ]]; then
    echo "${test_path}: Compilation Returned ${actual_compiler_exit_code}, not ${compiler_exit_code}"
    continue
  fi

  ./assemble $assembly_file_path $out_file_path 2>&1 /dev/null
  if [[ $? -ne "0" ]]; then
    echo "${test_path}: Assembly failed"
    continue
  fi

  actual_program_stdout=$(./${out_file_path})
  actual_program_exit_code=$?

  if [[ $actual_program_stdout -ne $program_stdout ]]; then
    echo "${test_path}: Unexpected stdout"
    echo "got:"
    echo $actual_program_stdout
    echo "expected"
    echo $program_stdout
    continue
  fi

  if [[ $actual_program_exit_code -ne $program_exit_code ]]; then
    echo "${test_path}: Got exit code {program_exit_code}, got {actual_program_exit_code}"
    continue
  fi
done
