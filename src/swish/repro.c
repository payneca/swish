#define _POSIX_C_SOURCE 200809L

#include <signal.h>
#include <stdio.h>
#include <string.h>
#include <unistd.h>

#include "uv.h"

static uv_process_t child;
static uv_signal_t sigchld_watcher;

static int saw_unexpected_sigchld = 0;

static void on_sigchld(uv_signal_t *handle, int signum)
{
    int alive;

    (void)signum;

    fprintf(stderr, "application SIGCHLD callback invoked\n");

    /*
     * The child requested by our uv_spawn() is supposed to be `sleep 30`.
     * If it is still alive, this SIGCHLD must have come from some other
     * child.
     */
    alive = (child.pid > 0 && kill(child.pid, 0) == 0);

    if (alive) {
        fprintf(stderr,
                "BUG REPRODUCED: spawned child pid %d is still alive\n",
                child.pid);

        saw_unexpected_sigchld = 1;

        /*
         * We have demonstrated the issue. Stop our watcher so that the
         * SIGCHLD caused by terminating `sleep` doesn't confuse the output.
         */
        uv_signal_stop(handle);

        /*
         * Terminate the real child so the reproducer exits immediately
         * instead of waiting 30 seconds.
         */
        uv_process_kill(&child, SIGTERM);
    } else {
        fprintf(stderr,
                "SIGCHLD received, but spawned child is no longer alive\n");
    }
}

static void on_child_exit(uv_process_t *process,
                          int64_t exit_status,
                          int term_signal)
{
    fprintf(stderr,
            "spawned child exited: pid=%d status=%lld signal=%d\n",
            process->pid,
            (long long)exit_status,
            term_signal);

    uv_close((uv_handle_t *)process, NULL);
    uv_close((uv_handle_t *)&sigchld_watcher, NULL);
}

int main(void)
{
    uv_loop_t *loop;
    uv_process_options_t options;
    char *args[] = {
        "/bin/sleep",
        "30",
        NULL
    };
    int rc;

    fprintf(stderr, "libuv version: %s\n", uv_version_string());

    loop = uv_default_loop();

    /*
     * Application-level SIGCHLD watcher.
     *
     * This is intentionally separate from libuv's private child watcher.
     */
    rc = uv_signal_init(loop, &sigchld_watcher);
    if (rc != 0) {
        fprintf(stderr,
                "uv_signal_init: %s\n",
                uv_strerror(rc));
        return 1;
    }

    rc = uv_signal_start(&sigchld_watcher, on_sigchld, SIGCHLD);
    if (rc != 0) {
        fprintf(stderr,
                "uv_signal_start: %s\n",
                uv_strerror(rc));
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
        fprintf(stderr,
                "uv_spawn: %s\n",
                uv_strerror(rc));
        return 1;
    }

    fprintf(stderr,
            "uv_spawn returned successfully; child pid=%d\n",
            child.pid);

    uv_run(loop, UV_RUN_DEFAULT);

    fprintf(stderr,
            "%s\n",
            saw_unexpected_sigchld
                ? "RESULT: unexpected SIGCHLD observed"
                : "RESULT: unexpected SIGCHLD NOT observed");

    return saw_unexpected_sigchld ? 0 : 2;
}
