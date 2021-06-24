#ifndef LLRT_RT_H
#define LLRT_RT_H

#include <stdio.h>
#include <inttypes.h>
#include <stdnoreturn.h>
#include <dirent.h>

typedef struct {
  char *ptr;
  uint64_t len;
} rt_string;

typedef struct {
  uint64_t argc;
  rt_string *argv;
} rt_args;

void llrt_init(int argc, char *argv[]);
rt_args llrt_args();

noreturn void llrt_panic(rt_string msg);
void llrt_exit(int32_t exitcode);

typedef struct {
  int32_t err;
  int32_t pid;
  FILE *cin;
  FILE *cout;
  FILE *cerr;
} rt_process;

rt_process llrt_process(const char *name, char *const argv[]);
int32_t llrt_wait(int32_t pid);

double llrt_time();

rt_string llrt_getcwd();

rt_string llrt_string_genid();
int32_t llrt_string_eq(rt_string a, rt_string b);
int32_t llrt_string_cmp(rt_string a, rt_string b);
rt_string llrt_string_concat(rt_string a, rt_string b);

rt_string llrt_f32_to_string(float a);
rt_string llrt_f64_to_string(double a);
rt_string llrt_i64_to_string(uint8_t radix, int64_t value);
rt_string llrt_u64_to_string(uint8_t radix, uint64_t value);

typedef struct {
  int32_t success;
  int64_t value;
} to_i64;

typedef struct {
  int32_t success;
  uint64_t value;
} to_u64;

typedef struct {
  int32_t success;
  float value;
} to_f32;

typedef struct {
  int32_t success;
  double value;
} to_f64;

to_i64 llrt_string_to_i64(uint8_t radix, rt_string s);
to_u64 llrt_string_to_u64(uint8_t radix, rt_string s);
to_f32 llrt_string_to_f32(rt_string s);
to_f64 llrt_string_to_f64(rt_string s);

rt_string llrt_readdir(DIR *dir);

void **llrt_stdin();
void **llrt_stdout();
void **llrt_stderr();

int32_t llrt_current_errno();

uint64_t llrt_xxh_seed();

#endif
