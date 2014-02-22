#!/usr/bin/env python3
from pandocfilters import toJSONFilter, RawInline, Para, Space, Math, BulletList
import sys

should_act = False

noindent = RawInline('latex', r'\noindent')

def mymargin(key, value, format, meta):
	global should_act
	if key == 'CodeBlock':
		should_act = True
	if key == 'Math':
		f, code = value
		if f['t'] == 'DisplayMath':
			should_act = True
	if key == 'Para' and should_act:
		should_act = False
		return Para([noindent, Space()] + value)

if __name__ == "__main__":
	toJSONFilter(mymargin)
