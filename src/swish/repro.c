#include <signal.h>
#include <stdio.h>
#include <string.h>

#include "uv.h"

static uv_process_t child;
static uv_signal_t sigchld_watcher;
static uv_timer_t timer;

static int unexpected_sigchld = 0;

static void on_child_exit(uv_process_t *process,
                          int64_t exit_status,
                          int term_signal)
{
  fprintf(stderr,
          "requested child exit_cb: pid=%d status=%lld signal=%d\n",
          process->pid,
          (long long) exit_status,
          term_signal);

  uv_close((uv_handle_t *) process, NULL);
}

static void cleanup(void)
{
  int rc;
  rc = uv_process_kill(&child, SIGTERM);
  if (rc != 0)
    fprintf(stderr, "uv_process_kill: %s\n", uv_strerror(rc));
}

static void on_sigchld(uv_signal_t *handle, int signum)
{
  (void) signum;
  fprintf(stderr,
          "UNEXPECTED: SIGCHLD arrived while requested child "
          "should still be sleeping\n");

  unexpected_sigchld = 1;

  /* Test is over. Cancel the watchdog and stop observing SIGCHLD. */
  uv_timer_stop(&timer);
  uv_close((uv_handle_t *) &timer, NULL);
  uv_signal_stop(handle);
  uv_close((uv_handle_t *) handle, NULL);
  cleanup();
}

static void on_timer(uv_timer_t *handle)
{
  fprintf(stderr, "timeout: no unexpected SIGCHLD observed\n");

  /*
   * Stop observing SIGCHLD first, so the deliberate termination below
   * cannot be mistaken for the condition we're testing.
   */
  uv_signal_stop(&sigchld_watcher);
  uv_close((uv_handle_t *) &sigchld_watcher, NULL);
  uv_timer_stop(handle);
  uv_close((uv_handle_t *) handle, NULL);
  cleanup();
}

int main(void)
{
  uv_loop_t *loop;
  uv_process_options_t options;
  char *args[] = { "/bin/sleep", "30", NULL };
  int rc;

  fprintf(stderr, "libuv version: %s\n", uv_version_string());

  loop = uv_default_loop();
  rc = uv_signal_init(loop, &sigchld_watcher);
  if (rc != 0) {
    fprintf(stderr, "uv_signal_init: %s\n", uv_strerror(rc));
    return 1;
  }

  rc = uv_signal_start(&sigchld_watcher, on_sigchld, SIGCHLD);
  if (rc != 0) {
    fprintf(stderr, "uv_signal_start: %s\n", uv_strerror(rc));
    return 1;
  }

  memset(&child, 0, sizeof(child));
  memset(&options, 0, sizeof(options));

  options.file = args[0];
  options.args = args;
  options.exit_cb = on_child_exit;

  fprintf(stderr, "calling uv_spawn()\n");

  rc = uv_spawn(loop, &child, &options);
  if (rc != 0) {
    fprintf(stderr, "uv_spawn: %s\n", uv_strerror(rc));
    return 1;
  }

  fprintf(stderr,
          "uv_spawn returned; requested child pid=%d\n",
          child.pid);

  /*
   * This timer is only a watchdog. If no unexpected SIGCHLD appears
   * promptly, terminate the sleeping child so the test can finish.
   */
  rc = uv_timer_init(loop, &timer);
  if (rc != 0) {
    fprintf(stderr, "uv_timer_init: %s\n", uv_strerror(rc));
    return 1;
  }

  rc = uv_timer_start(&timer, on_timer, 1000, 0);
  if (rc != 0) {
    fprintf(stderr, "uv_timer_start: %s\n", uv_strerror(rc));
    return 1;
  }

  uv_run(loop, UV_RUN_DEFAULT);

  fprintf(stderr,
          "RESULT: %s\n",
          unexpected_sigchld
          ? "unexpected SIGCHLD observed"
          : "unexpected SIGCHLD not observed");

  return unexpected_sigchld ? 0 : 2;
}
