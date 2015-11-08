#!/usr/bin/python

import sys

def strtol(k):
	ks = []
	for x in k:
		assert x[0] == '"'
		assert x[-1] == '"'
		x = x[1:-1]
		ks.append(x)
	return ks

def hn(s):
	assert s[0]=='{'
	assert s[-1]=='}'
	s = s[1:-1]
	pairs = s.split(",")
	k = [p.split(":")[0] for p in pairs]
	return strtol(k)

def printl(prefix,suffix,l):
	for e in l:
		print prefix+e+suffix

#printl("(def-new-node-ww ", ")", hn('{"htonr":"htonr_ui","ntohr":"htonr_ui","str":"str_i","truncr_f":"truncr_f_i","truncr_d":"truncr_d_i"}'))

if sys.argv[0] == "./hashtolisp.py":
	printl(sys.argv[1], sys.argv[2], hn(sys.argv[3]))
elif sys.argv[0] == "./listtolisp.py":
	l = sys.argv[3]
	assert l[0] == "[" and l[-1] == "]"
	l = l[1:-1].split(",")
	print l
	printl(sys.argv[1], sys.argv[2], strtol(l))
else:
	print "unknown program name", sys.argv[0]
