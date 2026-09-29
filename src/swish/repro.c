#define _POSIX_C_SOURCE 200809L

#include <signal.h>
#include <stdio.h>
#include <string.h>
#include <unistd.h>

#include "uv.h"

static volatile sig_atomic_t sigchld_count = 0;

static void sigchld_handler(int signo)
{
    (void) signo;
    sigchld_count++;

    /* write() is async-signal-safe; printf() is not. */
    static const char msg[] = "SIGCHLD received\n";
    write(STDERR_FILENO, msg, sizeof(msg) - 1);
}

static void on_exit(uv_process_t *process,
                    int64_t exit_status,
                    int term_signal)
{
    fprintf(stderr,
            "requested child exited: pid=%d status=%lld signal=%d\n",
            process->pid,
            (long long) exit_status,
            term_signal);

    uv_close((uv_handle_t *) process, NULL);
}

int main(void) {
    fprintf(stderr, "libuv version: %s\n", uv_version_string());

    struct sigaction sa;
    uv_loop_t *loop = uv_default_loop();
    uv_process_t child;
    uv_process_options_t options = {0};

    char *args[] = {
        "sleep",
        "30",
        NULL
    };

    memset(&sa, 0, sizeof(sa));
    sa.sa_handler = sigchld_handler;
    sigemptyset(&sa.sa_mask);

    if (sigaction(SIGCHLD, &sa, NULL) != 0) {
        perror("sigaction");
        return 1;
    }

    fprintf(stderr, "SIGCHLD count before uv_spawn: %d\n",
            (int) sigchld_count);

    options.file = "sleep";
    options.args = args;
    options.exit_cb = on_exit;

    int rc = uv_spawn(loop, &child, &options);
    if (rc != 0) {
        fprintf(stderr, "uv_spawn: %s\n", uv_strerror(rc));
        return 1;
    }

    fprintf(stderr,
            "uv_spawn returned; requested child pid=%d\n",
            child.pid);

    /*
     * Give any unexpected SIGCHLD a moment to arrive.
     * The requested `sleep 30` process should still be alive.
     */
    sleep(1);

    if (kill(child.pid, 0) == 0)
        fprintf(stderr, "requested child is still alive\n");
    else
        perror("kill(child.pid, 0)");

    fprintf(stderr, "SIGCHLD count after uv_spawn: %d\n",
            (int) sigchld_count);

    return uv_run(loop, UV_RUN_DEFAULT);
}
