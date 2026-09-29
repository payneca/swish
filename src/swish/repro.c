#define _POSIX_C_SOURCE 200809L

#include <signal.h>
#include <stdio.h>
#include <string.h>
#include <unistd.h>

#include "uv.h"

static volatile sig_atomic_t sigchld_count = 0;

static void sigchld_handler(int signo)
{
    static const char msg[] = "SIGCHLD received\n";

    (void) signo;
    sigchld_count++;

    write(STDERR_FILENO, msg, sizeof(msg) - 1);
}

static void on_exit(uv_process_t *process,
                    int64_t exit_status,
                    int term_signal)
{
    fprintf(stderr,
            "exit callback: pid=%d status=%lld signal=%d\n",
            process->pid,
            (long long) exit_status,
            term_signal);

    uv_close((uv_handle_t *) process, NULL);
}

int main(void)
{
    struct sigaction sa;
    uv_loop_t *loop;
    uv_process_t child;
    uv_process_options_t options;
    int rc;

    printf("libuv version: %s\n", uv_version_string());

    memset(&sa, 0, sizeof(sa));
    sa.sa_handler = sigchld_handler;
    sigemptyset(&sa.sa_mask);

    if (sigaction(SIGCHLD, &sa, NULL) != 0) {
        perror("sigaction");
        return 1;
    }

    loop = uv_default_loop();

    memset(&child, 0, sizeof(child));
    memset(&options, 0, sizeof(options));

    /*
     * Deliberately nonexistent executable.
     */
    char *args[] = {
        "program-that-had-better-not-exist",
        NULL
    };

    options.file = args[0];
    options.args = args;
    options.exit_cb = on_exit;

    fprintf(stderr, "SIGCHLD before uv_spawn: %d\n",
            (int) sigchld_count);

    rc = uv_spawn(loop, &child, &options);

    fprintf(stderr,
            "uv_spawn returned %d (%s)\n",
            rc,
            uv_strerror(rc));

    fprintf(stderr, "SIGCHLD after uv_spawn: %d\n",
            (int) sigchld_count);

    uv_run(loop, UV_RUN_DEFAULT);

    fprintf(stderr, "SIGCHLD after uv_run: %d\n",
            (int) sigchld_count);

    return 0;
}
