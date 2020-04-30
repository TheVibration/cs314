"""Prolog interpreter

Usage:
	main.py [<input_file>]
"""
import sys

from final import Interpreter

if __name__ == '__main__':
	if (len(sys.argv) <= 1):
		input_file = 'examples/starkhouse.pl'
	else:
		input_file = sys.argv[1]

	interpreter = Interpreter()

	# Test Problem 1
	interpreter.test_occurs_check()

	# Test Problem 2
	interpreter.test_variables_of_term()
	interpreter.test_variables_of_clause()

	# Test Problem 3
	interpreter.test_substitute_in_term()
	interpreter.test_substitute_in_clause()

	# Test Problem 4:
	interpreter.test_unify()

	# Test Problem 5:
	interpreter.test_nondet_query(input_file)
