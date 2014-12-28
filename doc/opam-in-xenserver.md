
XenServer uses the
[Xen project's](http://www.xenproject.org/)
[Xapi toolstack](http://www.xenproject.org/developers/teams/xapi.html)
to manage Resource Pools of Hosts running VMs.
The Xapi toolstack 

- manages clusters of Xen hosts with shared storage and networking
- allows running VMs to be migrated between hosts (with or without storage)
  with minimal downtime
- automatically restarts VMs after host failure
  ([High Availability](http://xapi-project.github.io/features/HA/HA.html))
- allows cross-site [Disaster Recovery](http://xapi-project.github.io/features/DR/DR.html)
- simplifies maintainence through [Rolling Pool Upgrade](http://xapi-project.github.io/features/RPU/RPU.html)
- collects performance statistics for historical analysis and for alerting
- has a full-featured
  [XML-RPC based API](http://xapi-project.github.io/xen-api/),
  used by clients such as
  [XenCenter](https://github.com/xenserver/xenadmin),
  [Xen Orchestra](https://xen-orchestra.com),
  [OpenStack](http://www.openstack.org)
  and [CloudStack](http://cloudstack.apache.org)

The Xapi toolstack is built from a large set of libraries and components
which are
developed independenly and versioned separately. This
makes sharing code with other open-source projects (like Mirage) very easy.
However this flexibility comes 
with a cost: when xapi depends on 45 separate libraries, how do I set up a build environment?
Exactly which libraries do I need?
If I change one of these libraries (e.g. to make a bugfix), exactly which bits should I rebuild?
This is where [opam](https://opam.ocaml.org),
the source package manager, can help.

Installing a toolstack build environment with opam is very easy. 
For example in a CentOS 6.5 VM,
first [install opam](https://opam.ocaml.org/doc/Install.html):

and then:
```
$ opam init --comp=4.01.0
$ eval `opam config env`
$ opam remote add xapi-project git://github.com/xapi-project/opam-repo-dev
```

Next install the necessary C libraries and development tools for xapi
using a command like

```
$ sudo yum install `opam install xapi -e centos`
```

Finally to build xapi itself:
```
$ opam install xapi
The following actions will be performed:
 - install   obuild.0.1.1                          [required by nbd, cdrom]
 - install   camlp4.4.01.0                         [required by cstruct]
 - install   ocamlfind.1.5.5                       [required by xapi]
 - install   base-no-ppx.base                      [required by lwt]
 - install   xmlm.1.2.0                            [required by xapi-libs-transitional, rpc]
 - install   uuidm.0.9.5                           [required by xapi-forkexecd]
 - install   type_conv.111.13.00                   [required by rpc]
 - install   syslog.1.4                            [required by xapi-forkexecd]
 - install   ssl.0.4.7                             [required by xapi]
 - install   re.1.2.2                              [required by xapi-forkexecd, tar-format]
 - install   ounit.2.0.0                           [required by xapi]
 - install   omake.0.9.8.6-0.rc1                   [required by xapi]
 - install   oclock.0.4.0                          [required by xapi]
 - install   libvhd.0.9.0                          [required by xapi]
 - install   fd-send-recv.1.0.1                    [required by xapi]
 - install   cppo.1.1.2                            [required by ocplib-endian]
 - install   cmdliner.0.9.6                        [required by tar-format, nbd]
 - install   cdrom.0.9.1                           [required by xapi]
 - install   base-bytes.legacy                     [required by ctypes]
 - install   sexplib.111.25.00                     [required by cstruct]
 - install   fieldslib.109.20.03                   [required by cohttp]
 - install   lwt.2.4.6                             [required by tar-format, nbd, rpc]
 - install   stringext.1.2.0                       [required by uri]
 - install   ocplib-endian.0.8                     [required by cstruct]
 - install   ctypes.0.3.4                          [required by opasswd]
 - install   xenctrl.0.9.28                        [required by xapi]
 - install   rpc.1.5.1                             [required by xapi]
 - install   uri.1.7.2                             [required by xapi-idl]
 - install   cstruct.1.5.0                         [required by tar-format, nbd]
 - install   opasswd.0.9.3                         [required by xapi]
 - install   xapi-backtrace.0.2                    [required by xapi-idl]
 - install   cohttp.0.10.1                         [required by xapi-idl]
 - install   xenstore.1.2.5                        [required by xapi]
 - install   tar-format.0.2.1                      [required by xapi]
 - install   nbd.1.0.2                             [required by xapi]
 - install   io-page.1.2.0                         [required by xen-gnt]
 - install   crc.0.9.0                             [required by xapi-rrd-transport]
 - install   xapi-stdext.0.13.0                    [required by xapi]
 - install   message-switch.0.10.4                 [required by xapi-idl]
 - install   xenstore_transport.0.9.4              [required by xapi-libs-transitional]
 - install   mirage-profile.0.4                    [required by xen-gnt]
 - install   xapi-rrd.0.9.1                        [required by xapi-rrd-transport, xapi-idl]
 - install   xapi-inventory.0.9.1                  [required by xapi]
 - install   xen-gnt.2.1.0                         [required by xapi-rrd-transport]
 - install   xapi-idl.0.9.21                       [required by xapi]
 - install   xapi-rrd-transport.0.7.2              [required by xapi-rrdd-plugin]
 - install   xapi-forkexecd.0.9.2                  [required by xapi]
 - install   xapi-tapctl.0.9.2                     [required by xapi]
 - install   xapi-netdev.0.9.1                     [required by xapi]
 - install   xapi-libs-transitional.0.9.7          [required by xapi]
 - install   xapi-rrdd-plugin.0.6.1                [required by xapi]
 - install   xapi.1.9.58
=== 52 to install ===
Do you want to continue ? [Y/n]
```

Opam also makes iterative development very easy.
Consider a schenario where a
[common interface](https://github.com/xapi-project/xcp-idl) has to be changed.
Without opam we have to figure out which components to rebuild manually--
this is both time-consuming and error-prone. Opam allows a package
to be "pinned" to a particular version (e.g. a local checkout) and will
take care of rebuilding only the dependent packages:

```
$ opam pin add xapi-idl /my/local/checkout
$ opam install xapi
...
xapi-idl needs to be reinstalled.
The following actions will be performed:
 - recompile xapi-idl.0.9.21*
 - recompile xapi-rrd-transport.0.7.2              [uses xapi-idl]
 - recompile xapi-forkexecd.0.9.2                  [uses xapi-idl]
 - recompile xapi-tapctl.0.9.2                     [uses xapi-forkexecd]
 - recompile xapi-netdev.0.9.1                     [uses xapi-forkexecd]
 - recompile xapi-libs-transitional.0.9.7          [uses xapi-forkexecd]
 - recompile xapi-rrdd-plugin.0.6.1                [uses xapi-idl]
 - recompile xapi.1.9.58                           [uses xapi-idl]
=== 8 to reinstall ===
Do you want to continue ? [Y/n]
```

It's important to be able to iterate quickly when testing a bugfix--
opam makes this easy too. After making a change to a "pinned" repository
the user just has to type

```
$ opam update
$ opam upgrade
...
```

and only the affected components will be rebuilt.

The Xapi toolstack shares lots of code with other projects which is great
because everyone benefits when someone improves the shared code.
However the large number of dependencies makes it tricky to build
the software completly by hand-- this is where opam helps. Opam manages
the dependencies and performs incremental rebuilds for us, making the
developer workflow much simpler.
