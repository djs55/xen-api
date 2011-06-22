#!/usr/bin/env python
#
# Copyright (C) Citrix Inc
#
# Permission to use, copy, modify, and distribute this software for any
# purpose with or without fee is hereby granted, provided that the above
# copyright notice and this permission notice appear in all copies.
#
# THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
# WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
# MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
# ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
# WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
# ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
# OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.

# Example storage backend using SMAPIv2 using raw files and Linux losetup

# WARNING: this API is considered to be unstable and may be changed at-will

import os, sys, commands

from smapiv2 import log, start, BackendError

root = "/sr/"

# [run task cmd] executes [cmd], throwing a BackendError if exits with
# a non-zero exit code.
def run(task, cmd):
    code, output = commands.getstatusoutput(cmd)
    if code <> 0:
        log("%s: %s exitted with code %d: %s" % (task, cmd, code, output))
        raise (BackendError("command failed", [ str(code), output ]))
    log("%s: %s" % (task, cmd))
    return output

# Use Linux "losetup" to create block devices from files
class Loop:
    # [_find task path] returns the loop device associated with [path]
    def _find(self, task, path):
        global root
        for line in run(task, "losetup -a").split("\n"):
            bits = line.split()
            loop = bits[0][0:-1]
            this_path = bits[2][1:-1]
            if this_path == path:
                return loop
        return None
    # [add task path] creates a new loop device for [path] and returns it
    def add(self, task, path):
        run(task, "losetup -f %s" % path)
        return self._find(task, path)
    # [remove task path] removes the loop device associated with [path]
    def remove(self, task, path):
        loop = self._find(task, path)
        run(task, "losetup -d %s" % loop)

# Use FreeBSD "mdconfig" to create block devices from files
class Mdconfig:
    # [_find task path] returns the unit (mdX) associated with [path]
    def _find(self, task, path):
        # md0	vnode	 1024M	/root/big.img
        for line in run(task, "mdconfig -l -v").split("\n"):
            bits = line.split()
            this_path = bits[3]
            if this_path == path:
                return bits[0] # md0
        return None
    # [add task path] returns a block device associated with [path]
    def add(self, task, path):
        return "/dev/" + run(task, "mdconfig -a -t vnode -f %s" % path)
    # [remove task path] removes the block device associated with [path]
    def remove(self, task, path):
        md = self._find(task, path)
        run(task, "mdconfig -d -u %s" % md) 

# [path_of_vdi vdi] returns the path in the local filesystem corresponding
# to vdi location [vdi]
def path_of_vdi(vdi):
    global root
    return root + vdi

class RawFiles:
    def __init__(self, device):
        self.device = device

    def query(self):
        return { "name": "RawFiles",
                 "vendor": "XCP",
                 "version": "0.1",
                 "features": [ feature_vdi_create,
                               feature_vdi_destroy,
                               feature_vdi_attach,
                               feature_vdi_detach,
                               feature_vdi_activate,
                               feature_vdi_deactivate ] }

    def sr_attach(self, task, sr):
        if not(os.path.exists(root)):
            raise BackendError("SR directory doesn't exist", [ root ])
    def sr_detach(self, task, sr):
        pass
    def sr_destroy(self, task, sr):
        pass

    def vdi_create(self, task, sr, name_label, name_description, virtual_size, ty, params):
        filename = run(task, "uuidgen") + ".raw"
        run(task, "dd if=/dev/zero of=%s bs=1 count=0 seek=%Ld" % (path_of_vdi(filename), virtual_size))
        return (filename, virtual_size)
    def vdi_destroy(self, task, sr, vdi):
        run(task, "rm -f %s" % (path_of_vdi(vdi)))

    def vdi_attach(self, task, dp, sr, vdi, read_write):
        path = path_of_vdi(vdi)
        loop = self.device.add(task, path)
        return loop

    def vdi_activate(self, task, dp, sr, vdi):
        pass
    def vdi_deactivate(self, task, dp, sr, vdi):
        pass
    def vdi_detach(self, task, dp, sr, vdi):
        path = path_of_vdi(vdi)
        self.device.remove(task, path)
        
if __name__ == "__main__":
    from optparse import OptionParser

    parser = OptionParser()
    parser.add_option("-l", "--log", dest="logfile", help="log to LOG", metavar="LOG")
    parser.add_option("-p", "--port", dest="port", help="listen on PORT", metavar="PORT")
    parser.add_option("-i", "--ip-addr", dest="ip", help="listen on IP", metavar="IP")
    (options, args) = parser.parse_args()
    if options.logfile:
        from smapiv2 import reopenlog
        reopenlog(options.logfile)
    if not options.ip and not options.ip:
        print >>sys.stderr, "Need an --ip-addr and --port. Use -h for help"
        sys.exit(1)

    ip = options.ip
    port = int(options.port)

    arch = run("startup", "uname")
    if arch == "Linux":
        log("startup: Using loop devices")
        start(RawFiles(Loop()), ip, port)
    elif arch == "FreeBSD":
        log("startup: Using mdconfig devices")
        start(RawFiles(Mdconfig()), ip, port)
    else:
        log("startup: Unknown architecture: %s" % arch)
