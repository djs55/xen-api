#!/usr/bin/env python

import os, sys, socket, traceback

ip = '169.254.0.2' # XXX
port = 8080 # XXX

def log(txt):
    print txt
    sys.stdout.flush()

# Functions to construct SMAPI return types #################################

unit = [ "Success", "Unit" ]

def newvdi(location, virtual_size):
    return ['Success', ['NewVdi', {'vdi': location, 'virtual_size': str(virtual_size) }]]

def attachedvdi(xenstore_keys):
    # NB domain is ignored
    return ['Success', ['Vdi', {'backend_domain': '0', 'xenstore_keys': xenstore_keys }]]

def value(result):
    return { "Status": "Success", "Value": result }

def backend_error(code, params):
    return [ "Failure", [ "Backend_error", code, params ] ]

def internal_error(txt):
    return [ "Failure", "Internal_error", txt ]

# Throw this to return an SR_BACKEND_FAILURE to the caller ##################

class BackendError(Exception):
    def __init__(self, code, params):
        self.code = code
        self.params = params
    def __str__(self):
        return "BackendError(%s, %s)" % (self.code, ", ".join(self.params))

# Type-checking helper functions ############################################

def expect_none(x):
    if x <> None:
        raise (BackendError("type error", [ "None", repr(x) ]))

def expect_long(x):
    if type(x) <> type(0L):
        raise (BackendError("type error", [ "long int", repr(x) ]))

def expect_string(x):
    if type(x) <> type(""):
        raise (BackendError("type error", [ "string", repr(x) ]))

# Well-known feature flags understood by xapi ##############################

feature_sr_probe = "SR_PROBE"
feature_sr_update = "SR_UPDATE"
feature_sr_supports_local_caching = "SR_SUPPORTS_LOCAL_CACHING"
feature_vdi_create = "VDI_CREATE"
feature_vdi_delete = "VDI_DELETE"
feature_vdi_attach = "VDI_ATTACH"
feature_vdi_detach = "VDI_DETACH"
feature_vdi_resize = "VDI_RESIZE"
feature_vdi_resize_online = "VDI_RESIZE_ONLINE"
feature_vdi_clone = "VDI_CLONE"
feature_vdi_snapshot = "VDI_SNAPSHOT"
feature_vdi_activate = "VDI_ACTIVATE"
feature_vdi_deactivate = "VDI_DEACTIVATE"
feature_vdi_update = "VDI_UPDATE"
feature_vdi_introduce = "VDI_INTRODUCE"
feature_vdi_generate_config = "VDI_GENERATE_CONFIG"
feature_vdi_reset_on_boot = "VDI_RESET_ON_BOOT"

# Unmarshals arguments and marshals results (including exceptions) ##########

class Marshall:
    def __init__(self, x):
        self.x = x

    def query(self, args):
        result = self.x.query()
        return value(result)

    def sr_attach(self, args):
        result = self.x.sr_attach(args["task"], args["sr"])
        expect_none(result)
        return value(unit)
    def sr_detach(self, args):
        result = self.x.sr_detach(args["task"], args["sr"])
        expect_none(result)
        return value(unit)
    def sr_destroy(self, args):
        result = self.x.sr_destroy(args["task"], args["sr"])
        expect_none(result)
        return value(unit)     

    def vdi_create(self, args):
        location, virtual_size = self.x.vdi_create(args["task"], args["sr"], args["name_label"], args["name_description"], long(args["virtual_size"]), args["ty"], args["params"])
        expect_string(location)
        expect_long(virtual_size)
        return value(newvdi(location, virtual_size))
    def vdi_destroy(self, args):
        result = self.x.vdi_destroy(args["task"], args["sr"], args["vdi"])
        expect_none(result)
        return value(unit)

    def vdi_attach(self, args):
        result = self.x.vdi_attach(args["task"], args["dp"], args["sr"], args["vdi"], args["read_write"])
        expect_string(result)
        xenstore_keys = { "params": result }
        return value(attachedvdi(xenstore_keys))
    def vdi_activate(self, args):
        result = self.x.vdi_activate(args["task"], args["dp"], args["sr"], args["vdi"])
        expect_none(result)
        return value(unit)
    def vdi_deactivate(self, args):
        result = self.x.vdi_deactivate(args["task"], args["dp"], args["sr"], args["vdi"])
        expect_none(result)
        return value(unit)
    def vdi_detach(self, args):
        result = self.x.vdi_detach(args["task"], args["dp"], args["sr"], args["vdi"])
        expect_none(result)
        return value(unit)


    def _dispatch(self, method, params):
        try:
            log("method = %s params = %s" % (method, repr(params)))
            args = params[0]
            if method == "query":
                return self.query(args)
            elif method == "SR.attach":
                return self.sr_attach(args)
            elif method == "SR.detach":
                return self.sr_detach(args)
            elif method == "VDI.create":
                return self.vdi_create(args)
            elif method == "VDI.destroy":
                return self.vdi_destroy(args)
            elif method == "VDI.attach":
                return self.vdi_attach(args)
            elif method == "VDI.activate":
                return self.vdi_activate(args)
            elif method == "VDI.deactivate":
                return self.vdi_deactivate(args)
            elif method == "VDI.detach":
                return self.vdi_detach(args)
        except BackendError, e:
            log("caught %s" % e)
            traceback.print_exc()
            return value(backend_error(e.code, e.params))
        except Exception, e:
            log("caught %s" % e)
            traceback.print_exc()
            return value(internal_error(str(e)))

# SimpleXMLRPCServer with SO_REUSEADDR ######################################

from SimpleXMLRPCServer import SimpleXMLRPCServer
class Server(SimpleXMLRPCServer):
    def __init__(self, *args):
        SimpleXMLRPCServer.__init__(self, *args)
    def server_bind(self):
        self.socket.setsockopt(socket.SOL_SOCKET, socket.SO_REUSEADDR, 1)
        SimpleXMLRPCServer.server_bind(self)

# This is a hack to patch slow socket.getfqdn calls that
# BaseHTTPServer (and its subclasses) make.
# See: http://bugs.python.org/issue6085
# See: http://www.answermysearches.com/xmlrpc-server-slow-in-python-how-to-fix/2140/
import BaseHTTPServer

def _bare_address_string(self):
    host, port = self.client_address[:2]
    return '%s' % host

BaseHTTPServer.BaseHTTPRequestHandler.address_string = \
        _bare_address_string


# Given an implementation, serve requests forever ###########################

def start(impl):
    server = Server((ip, port))
    log("server registered on %s:%d" % (ip, port))
    server.register_introspection_functions() # for debugging
    server.register_instance(Marshall(impl))
    log("serving requests forever")
    server.serve_forever()
