#!/bin/env python

from cffi import FFI
import os

ffibuilder = FFI()

ffibuilder.set_source(
    "_imf",
    r"""
    #include <stdlib.h>
    #include "HsFFI.h"
    #include "FFI_stub.h"

    HsBool imf_init(void) {
        int argc = 2;
        char *argv[] = { "+RTS", "-A32m", NULL };
        char **pargv = argv;

        hs_init(&argc, &pargv);

        return HS_BOOL_TRUE;
    }

    void imf_exit(void) {
        hs_exit();
    }
""",
    libraries=["imf"],
    library_dirs=["/usr/local/lib"],
    include_dirs=[
        "../../build/Data/IMF",
        f"{os.environ['HOME']}/.stack/programs/x86_64-linux/ghc-tinfo6-8.4.3/lib/ghc-8.4.3/include",
    ],
)

ffibuilder.cdef(
    r"""
    typedef void* HsStablePtr;
    void hs_free_stable_ptr (HsStablePtr ptr);
    void hs_free_cstring (char *cstr);

    _Bool imf_init(void);
    void imf_exit(void);

    HsStablePtr mailbox_parse(char *raw);
    char* mailbox_show(HsStablePtr mbox);
    bool mailbox_equals(HsStablePtr mbox1, HsStablePtr mbox2);
    HsStablePtr mailbox_from_parts(char *display, char *local, char *domain);
    char* mailbox_display(HsStablePtr mbox);
    char* mailbox_local(HsStablePtr mbox);
    char* mailbox_domain(HsStablePtr mbox);

    HsStablePtr mailbox_list_parse(char *raw);
    char* mailbox_list_show(HsStablePtr mboxes);
    int mailbox_list_length(HsStablePtr mboxes);
    HsStablePtr mailbox_list_item(HsStablePtr mboxes, int key);

    HsStablePtr datetime_parse(char *raw);
    char* datetime_show(HsStablePtr dtime);
    bool datetime_equals(HsStablePtr dtime1, HsStablePtr dtime2);
    HsStablePtr datetime_from_nanoseconds(long ns, int tzoffset);
    long datetime_nanoseconds(HsStablePtr dtime);
    int datetime_tzoffset(HsStablePtr dtime);

    HsStablePtr message_id_parse(char *raw);
    char* message_id_show(HsStablePtr msgid);
    bool message_id_equals(HsStablePtr msgid1, HsStablePtr msgid2);
    HsStablePtr message_id_from_parts(char *left, char *right);
    char* message_id_left(HsStablePtr msgid);
    char* message_id_right(HsStablePtr msgid);

    HsStablePtr header_parse(char *raw);
    char* header_show(HsStablePtr hf);
    bool header_equals(HsStablePtr hf1, HsStablePtr hf2);

    HsStablePtr header_list_parse(char *raw);
    char* header_list_show(HsStablePtr h);
    int header_list_length(HsStablePtr h);
    HsStablePtr header_list_item(HsStablePtr h, int key);

    HsStablePtr message_parse(char *raw);
    char* message_show(HsStablePtr msg);
    bool message_equals(HsStablePtr msg1, HsStablePtr msg2);
    HsStablePtr message_headers(HsStablePtr msg);
    char* message_body(HsStablePtr msg);
"""
)

if __name__ == "__main__":
    ffibuilder.compile(tmpdir="python/imf", verbose=True, target="_imf.*")
