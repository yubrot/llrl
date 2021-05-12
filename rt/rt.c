#define _GNU_SOURCE

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdint.h>
#include <inttypes.h>
#include <stdnoreturn.h>
#include <math.h>
#include <errno.h>
#include <unistd.h>
#include <dirent.h>
#include <fcntl.h>
#include <gc/gc.h>
#include <sys/random.h>
#include <sys/time.h>
#include <sys/wait.h>

void llrt_init_signal() {
  signal(SIGPIPE, SIG_IGN);
}

void llrt_restore_signal() {
  signal(SIGPIPE, SIG_DFL);
}

typedef struct {
  char *ptr;
  uint64_t len;
} rt_string;

typedef struct {
  uint64_t argc;
  rt_string *argv;
} rt_args;

static rt_args last_args = (rt_args){.argc = 0, .argv = NULL};

void llrt_init(int argc, char *argv[]) {
  llrt_init_signal();

  if (argc < 1) return;
  argc -= 1;
  argv = &argv[1];

  last_args.argc = (uint64_t)argc;
  last_args.argv = GC_malloc(sizeof(rt_string) * last_args.argc);
  for (int i = 0; i < argc; ++i) {
    rt_string s;
    s.len = strlen(argv[i]);
    s.ptr = GC_malloc(s.len);
    memcpy(s.ptr, argv[i], s.len);
    last_args.argv[i] = s;
  }
}

rt_args llrt_args() {
  return last_args;
}

noreturn void llrt_panic(rt_string msg) {
  if (msg.len != 0) {
    fwrite(msg.ptr, sizeof(char), msg.len, stderr);
    fputs("\n", stderr);
  }
  abort();
}

void llrt_exit(int32_t exitcode) {
  exit(exitcode);
}

typedef struct {
  int32_t err;
  int32_t pid;
  FILE *cin;
  FILE *cout;
  FILE *cerr;
} rt_process;

rt_process llrt_process(const char *name, char *const argv[]) {
  rt_process ret;
  int init[2]; // used to notify an execvp error
  int cin[2];
  int cout[2];
  int cerr[2];

  // TODO: Handle pipe error
  pipe2(init, O_CLOEXEC);
  pipe2(cin, O_CLOEXEC);
  pipe2(cout, O_CLOEXEC);
  pipe2(cerr, O_CLOEXEC);

  pid_t pid = fork();

  if (pid < 0) {
    // fork failed
    ret.err = errno;
    for (int i = 0; i < 2; ++i) {
      close(init[i]);
      close(cin[i]);
      close(cout[i]);
      close(cerr[i]);
    }
    return ret;
  }

  if (pid == 0) {
    // fork child
    close(init[0]);
    close(cin[1]);
    close(cout[0]);
    close(cerr[0]);

    llrt_restore_signal();

    dup2(cin[0], STDIN_FILENO);
    dup2(cout[1], STDOUT_FILENO);
    dup2(cerr[1], STDERR_FILENO);
    execvp(name, argv);

    // execvp failed: put error to init pipe
    char buf[4];
    *(int32_t *)buf = errno;
    write(init[1], buf, 4); // FIXME: Ensure that writing succeeds atomically
    exit(1);
  }

  // fork parent
  close(init[1]);
  close(cin[0]);
  close(cout[1]);
  close(cerr[1]);

  char buf[4];
  if (read(init[0], buf, 4) != 0) {
    // execvp failed
    ret.err = *(int32_t *)buf;
    close(init[0]);
    close(cin[1]);
    close(cout[0]);
    close(cerr[0]);
    return ret;
  }

  // execvp succeeded
  close(init[0]);
  ret.cin = fdopen(cin[1], "w");
  ret.cout = fdopen(cout[0], "r");
  ret.cerr = fdopen(cerr[0], "r");

  // FIXME: Error handling
  if (ret.cin == NULL || ret.cout == NULL || ret.cerr == NULL) abort();

  ret.pid = pid;
  ret.err = 0;
  return ret;
}

int32_t llrt_wait(int32_t pid) {
  int wstatus;
  if (waitpid(pid, &wstatus, 0) < 0) return -1;
  if (!WIFEXITED(wstatus)) return -1;
  return WEXITSTATUS(wstatus);
}

double llrt_time() {
  struct timeval t;
  gettimeofday(&t, NULL);
  return t.tv_sec + t.tv_usec * 1e-6;
}

rt_string llrt_getcwd() {
  char *buf = getcwd(NULL, 0); // Relies on POSIX.1-2001 extension
  rt_string ret;
  ret.len = strlen(buf);
  ret.ptr = GC_malloc(ret.len);
  memcpy(ret.ptr, buf, ret.len);
  free(buf);
  return ret;
}

static uint64_t last_sym;

rt_string llrt_string_genid() {
  last_sym += 1;
  rt_string ret;
  ret.len = 2 + (uint64_t)log10(last_sym); // '@' + index
  ret.ptr = GC_malloc(ret.len + 1); // sprintf writes 0 at the end
  sprintf(ret.ptr, "@%llu", (unsigned long long)last_sym);
  return ret;
}

int32_t llrt_string_eq(rt_string a, rt_string b) {
  return a.len == b.len && (a.len == 0 || memcmp(a.ptr, b.ptr, a.len) == 0);
}

int32_t llrt_string_cmp(rt_string a, rt_string b) {
  uint64_t len = a.len < b.len ? a.len : b.len;
  if (0 < len) {
    int32_t cmp = memcmp(a.ptr, b.ptr, len);
    if (cmp != 0) return cmp < 0 ? -1 : 1;
  }
  if (a.len != b.len) return a.len < b.len ? -1 : 1;
  return 0;
}

rt_string llrt_string_concat(rt_string a, rt_string b) {
  if (a.len == 0) return b;
  if (b.len == 0) return a;
  rt_string ret;
  ret.len = a.len + b.len;
  ret.ptr = GC_malloc(ret.len);
  memcpy(ret.ptr, a.ptr, a.len);
  memcpy(ret.ptr + a.len, b.ptr, b.len);
  return ret;
}

rt_string llrt_f32_to_string(float a) {
  char buf[64];
  sprintf(buf, "%.8g", a);
  rt_string ret;
  ret.len = strlen(buf);
  ret.ptr = GC_malloc(ret.len);
  memcpy(ret.ptr, buf, ret.len);
  return ret;
}

rt_string llrt_f64_to_string(double a) {
  char buf[64];
  sprintf(buf, "%.16g", a);
  rt_string ret;
  ret.len = strlen(buf);
  ret.ptr = GC_malloc(ret.len);
  memcpy(ret.ptr, buf, ret.len);
  return ret;
}

static rt_string zero_string = {.len = 1, .ptr = "0"};

char digit_to_char(uint8_t digit) {
  return digit < 10 ? '0' + digit : 'a' + (digit - 10);
}

rt_string llrt_i64_to_string(uint8_t radix, int64_t value) {
  if (value == 0) return zero_string;
  char buf[64];
  int64_t sign = value < 0 ? -1 : 1;
  size_t len = 0;
  while (value != 0) {
    buf[63 - len] = digit_to_char((uint8_t)((value % radix) * sign));
    value /= radix;
    len++;
  }
  if (sign < 0) {
    buf[63 - len] = '-';
    len++;
  }
  rt_string ret;
  ret.len = len;
  ret.ptr = GC_malloc(ret.len);
  memcpy(ret.ptr, &buf[64 - len], ret.len);
  return ret;
}

rt_string llrt_u64_to_string(uint8_t radix, uint64_t value) {
  if (value == 0) return zero_string;
  char buf[64];
  size_t len = 0;
  while (value != 0) {
    buf[63 - len] = digit_to_char((uint8_t)(value % radix));
    value /= radix;
    len++;
  }
  rt_string ret;
  ret.len = len;
  ret.ptr = GC_malloc(ret.len);
  memcpy(ret.ptr, &buf[64 - len], ret.len);
  return ret;
}

int8_t char_to_digit(char c) {
  return
    ('0' <= c && c <= '9')
      ? (c - '0')
      : ('a' <= c && c <= 'z')
        ? (c - 'a' + 10)
        : ('A' <= c && c <= 'Z')
          ? (c - 'A' + 10)
          : -1;
}

typedef struct {
  int32_t success;
  int64_t value;
} to_i64;

to_i64 llrt_string_to_i64(uint8_t radix, rt_string s) {
  size_t index;
  int64_t sign;
  if (s.len >= 2 && s.ptr[0] == '-') {
    index = 1;
    sign = -1;
  } else if (s.len >= 1) {
    index = 0;
    sign = 1;
  } else {
    return (to_i64){.success = 0};
  }
  int64_t value = 0;
  for (; index < s.len; index++) {
    int8_t digit = char_to_digit(s.ptr[index]);
    if (digit < 0 || radix <= digit) return (to_i64){.success = 0};
    if (sign < 0) {
      if ((INT64_MIN + digit) / radix > value) return (to_i64){.success = 0};
      value = value * radix - digit;
    } else {
      if ((INT64_MAX - digit) / radix < value) return (to_i64){.success = 0};
      value = value * radix + digit;
    }
  }
  return (to_i64){.success = 1, .value = value};
}

typedef struct {
  int32_t success;
  uint64_t value;
} to_u64;

to_u64 llrt_string_to_u64(uint8_t radix, rt_string s) {
  if (s.len == 0) return (to_u64){.success = 0};
  uint64_t value = 0;
  for (size_t index = 0; index < s.len; index++) {
    int8_t digit = char_to_digit(s.ptr[index]);
    if (digit < 0 || radix <= digit)
      return (to_u64){.success = 0};
    if ((UINT64_MAX - digit) / radix < value)
      return (to_u64){.success = 0};
    value = value * radix + digit;
  }
  return (to_u64){.success = 1, .value = value};
}

typedef struct {
  int32_t success;
  float value;
} to_f32;

to_f32 llrt_string_to_f32(rt_string s) {
  if (s.len == 0 || 64 <= s.len) return (to_f32){.success = 0};
  char buf[64];
  memcpy(buf, s.ptr, s.len);
  buf[s.len] = 0; // NULL-terminated
  errno = 0;
  char *end = NULL;
  float value = strtof(buf, &end);
  if (errno != 0 || end != buf + s.len) return (to_f32){.success = 0};
  return (to_f32){.success = 1, .value = value};
}

typedef struct {
  int32_t success;
  double value;
} to_f64;

to_f64 llrt_string_to_f64(rt_string s) {
  if (s.len == 0 || 64 <= s.len) return (to_f64){.success = 0};
  char buf[64];
  memcpy(buf, s.ptr, s.len);
  buf[s.len] = 0; // NULL-terminated
  errno = 0;
  char *end = NULL;
  double value = strtod(buf, &end);
  if (errno != 0 || end != buf + s.len) return (to_f64){.success = 0};
  return (to_f64){.success = 1, .value = value};
}

rt_string llrt_readdir(DIR *dir) {
  struct dirent *ent = readdir(dir);
  if (ent == NULL) return (rt_string){.len = 0, .ptr = NULL};
  rt_string ret;
  ret.len = strlen(ent->d_name);
  ret.ptr = GC_malloc(ret.len);
  memcpy(ret.ptr, ent->d_name, ret.len);
  return ret;
}

// NOTE: Actually this is not C89/C99-compatible
#undef stdin
#undef stdout
#undef stderr
static void **llrt_stdin_ref = (void **)&stdin;
static void **llrt_stdout_ref = (void **)&stdout;
static void **llrt_stderr_ref = (void **)&stderr;

void **llrt_stdin() {
  return llrt_stdin_ref;
}

void **llrt_stdout() {
  return llrt_stdout_ref;
}

void **llrt_stderr() {
  return llrt_stderr_ref;
}

int32_t llrt_current_errno() {
  return errno;
}

static uint64_t llrt_xxh_seed_cache;

uint64_t llrt_xxh_seed() {
  if (llrt_xxh_seed_cache == 0) {
    uint64_t tmp[1];
    getrandom((void *)tmp, sizeof(tmp), 0);
    llrt_xxh_seed_cache = tmp[0];
  }
  llrt_xxh_seed_cache += 1;
  return llrt_xxh_seed_cache;
}
