#!/usr/bin/env python

import os, sys, commands

def sr_attach(args):
	print "SR.attach task:%s sr:%s" % (args["task"], args["sr"])
	sys.stdout.flush()
	return { "Status": "Success", "Value": [ "Success", "Unit" ] }

def sr_detach(args):
	print "SR.detach task:%s sr:%s" % (args["task"], args["sr"])
	sys.stdout.flush()
	return { "Status": "Success", "Value": [ "Success", "Unit" ] }

def vdi_create(args):
	print "VDI.create task:%s sr:%s name_label:%s virtual_size:%s" % (args["task"], args["sr"], args["name_label"], args["virtual_size"])
	print "test"
	sys.stdout.flush()
	location = args["name_label"]
	print "location = %s" % location
	try:
		cmd = "/bin/dd if=/dev/zero of=/tmp/%s bs=1 count=0 seek=%s" % (args["name_label"], args["virtual_size"])
		print "%s" % cmd
		sys.stdout.flush()
		code, output = commands.getstatusoutput(cmd)
		print "%d, %s" % (code, output)
		sys.stdout.flush()
		if code == 0:
			print "VDI.create OK"
			sys.stdout.flush()
			return {'Status': 'Success', 'Value': ['Success', ['NewVdi', {'vdi': location, 'virtual_size': args["virtual_size"]}]]}
		else:
			print "VDI.destroy failed: %d %s" % (code, output)
			sys.stdout.flush()
			return {'Status': 'Success', 'Value': ['Failure', ['Backend_error', 'dd failed', []]]}
	except Exception, e:
		print "VDI.create %s" % (str(e))
		sys.stdout.flush()

def vdi_destroy(args):
	print "VDI.destroy task:%s sr:%s vdi:%s" % (args["task"], args["sr"], args["vdi"])
	sys.stdout.flush()
	try:
		cmd = "/bin/rm -f /tmp/%s" % (args["vdi"])
		code, output = commands.getstatusoutput(cmd)
		if code == 0:
			print "VDI.destroy OK"
			sys.stdout.flush()
			return { "Status": "Success", "Value": [ "Success", "Unit" ] }
		else:
			print "VDI.destroy failed: %d, %s" % (code, output)
			sys.stdout.flush()
			return {'Status': 'Success', 'Value': ['Failure', ['Backend_error', 'dd failed', []]]}
	except Exception, e:
		print "VDI.destroy %s" % (str(e))
		sys.stdout.flush()

def find_loop_device(path):
	path = "/tmp/" + path
	cmd = "/sbin/losetup -a"
	code, output = commands.getstatusoutput(cmd)
	if code == 0:
		for line in output.split("\n"):
			bits = line.split()
			loop = bits[0][0:-1]
			this_path = bits[2][1:-1]
			if this_path == path:
				return loop
		return None
	raise output

def add_loop_device(path):
	cmd = "/sbin/losetup -f /tmp/%s" % (path)
	code, output = commands.getstatusoutput(cmd)
	if code == 0:
		return find_loop_device(path)
	raise output

def del_loop_device(path):
	loop = find_loop_device(path)
	cmd = "/sbin/losetup -d %s" % loop
	code, output = commands.getstatusoutput(cmd)
	return

def vdi_attach(args):
	print "VDI.attach task:%s sr:%s vdi:%s" % (args["task"], args["sr"], args["vdi"])
	loop = add_loop_device(args["vdi"])
	print "loop = %s" % loop
	sys.stdout.flush()
	dev = os.stat(loop).st_rdev
	major = dev / 256
	minor = dev % 256
	physical_device = "%x:%x" % (major, minor)
	print "physical_device = %s" % physical_device
	sys.stdout.flush()
	return {'Status': 'Success', 'Value': ['Success', ['Vdi', {'backend_domain': '0', 'physical_device': physical_device }]]}

def vdi_activate(args):
	print "VDI.activate task:%s sr:%s vdi:%s" % (args["task"], args["sr"], args["vdi"])
	return { "Status": "Success", "Value": [ "Success", "Unit" ] }

def vdi_deactivate(args):	
	print "VDI.deactivate task:%s sr:%s vdi:%s" % (args["task"], args["sr"], args["vdi"])
	return { "Status": "Success", "Value": [ "Success", "Unit" ] }

def vdi_detach(args):
	print "VDI.detach task:%s sr:%s vdi:%s" % (args["task"], args["sr"], args["vdi"])
	del_loop_device(args["vdi"])
	return { "Status": "Success", "Value": [ "Success", "Unit" ] }

if __name__ == "__main__":
	res = vdi_create({"name_label":"foo", "virtual_size": "123", "sr": "sr", "task": "task"})
	print repr(res)
	loop = add_loop_device("foo")
	print "loop = %s" % loop
	del_loop_device("foo")
	print "OK"

from SimpleXMLRPCServer import SimpleXMLRPCServer

server = SimpleXMLRPCServer(('127.0.0.1', 8080))
server.register_introspection_functions()

server.register_function(sr_attach, "SR.attach")
server.register_function(sr_detach, "SR.detach")
server.register_function(vdi_create, "VDI.create")
server.register_function(vdi_destroy, "VDI.destroy")
server.register_function(vdi_attach, "VDI.attach")
server.register_function(vdi_detach, "VDI.detach")
server.register_function(vdi_activate, "VDI.activate")
server.register_function(vdi_deactivate, "VDI.deactivate")

if __name__ == "__main__":
	server.serve_forever()
