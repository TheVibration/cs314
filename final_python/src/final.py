from prolog_structures import Rule, RuleBody, Term, Function, Variable, Atom, Number
from prolog_parser import PrologParser
from typing import List
import sys
import lark

def read(file):
	with open(file) as f:
		return f.read()

class Not_unifiable(Exception):
	pass

'''
The interpreter reads a file or prolog program.
After it starts, it will accept queries (goals)

Read prolog_structures.py for data structures
representing Prolog terms, rules, and goals.
'''
class Interpreter:
	def __init__(self):
		pass

	'''
	Problem 1
	occurs_check (v, t) where v is of type Variable, t is of type Term.
	occurs_check (v, t) returns true if the Prolog Variable v occurs in t.
	Please see the lecture note Control in Prolog to revisit the concept of
	occurs-check.
	'''
	def occurs_check (self, v : Variable, t : Term) -> bool:
		pass

	# assert (occurs_check (var "E") (func "cons" [const "a"; const "b"; var "E"]))
	def test_occurs_check (self):
		v = Variable("E")
		t = Function("cons", [Atom("a"), Atom("b"), Variable("E")])
		#assert
		(self.occurs_check(v, t))

	'''
	Problem 2
	variables_of_term (t) where t is of type Term.
	variables_of_clause (c) where c is of type Rule.

	The function should return the Variables contained in a term or a rule
	using Python set.

	The result must be saved in a Python set. The type of each element (a Prolog Variable)
	in the set is Variable.
	'''
	def variables_of_term (self, t : Term) -> set :
  		pass

	def variables_of_clause (self, c : Rule) -> set :
		pass

	# The variables in a function f (X, Y, a) is [X; Y]
	def test_variables_of_term (self):
		t = Function("f", [Variable("X"), Variable("Y"), Atom("a")])
		#assert
		(self.variables_of_term (t) == set([Variable("X"), Variable("Y")]))

	# The variables in a Prolog rule p (X, Y, a) :- q (a, b, a) is [X; Y]
	def test_variables_of_clause (self):
		c = Rule (Function ("p", [Variable("X"), Variable("Y"), Atom("a")]),
					RuleBody ([Function ("q", [Atom("a"), Atom("b"), Atom("a")])]))
		#assert
		(self.variables_of_clause(c) == set([Variable("X"), Variable("Y")]))

	'''
	Problem 3
	substitute_in_term (s, t) where s is of type dictionary and t is of type Term
	substitute_in_clause (s, t) where s is of type dictionary and c is of type Rule,

	The value of type dict should be a Python dictionary whose key is of type Variable
	and value is of type Term in general. It is a map from variables to terms.

	The function should return t_ obtained by applying substitution s to t.

	Use Python dictionary to represent a subsititution map.
	'''
	def substitute_in_term (self, s : dict, t : Term) -> Term:
		pass

	def substitute_in_clause (self, s : dict, c : Rule) -> Rule:
  		pass

	# Function substitution - f (X, Y, a) [Y/0, X/Y] = f (Y, 0, a)
	def test_substitute_in_term (self):
		s = { Variable("Y"): Number(0), Variable("X"): Variable("Y") }
		t = Function ("f", [Variable("X"), Variable("Y"), Atom("a")])
		t_ = Function ("f", [Variable("Y"), Number(0), Atom("a")])
		#assert
		(self.substitute_in_term (s, t) == t_)

	# Given a Prolog rule, p (X, Y, a) :- q (a, b, a), after doing substitution [Y/0, X/Y],
    #            we have p (Y, 0, a) :- q (a, b, a)
	def test_substitute_in_clause (self):
		s = { Variable("Y"): Number(0), Variable("X"): Variable("Y") }
		p = Function ("p", [Variable("X"), Variable("Y"), Atom("a")])
		q = Function ("q", [Atom("a"), Atom("b"), Atom("a")])
		p_ = Function ("p", [Variable("Y"), Number(0), Atom("a")])
		q_ = Function ("q", [Atom("a"), Atom("b"), Atom("a")])
		r = Rule (p, [q])
		r_ = Rule (p_, [q_])
		#assert
		(self.substitute_in_clause(s, r) == r_)

	'''
	Problem 4
	unify (t1, t2) where t1 is of type term and t2 is of type Term
	The function should return a substitution map of type dict,
	which is a unifier of the given terms. You may find the pseudocode
	of unify in the lecture note Control in Prolog useful.

	The function should raise the exception raise Not_unfifiable (),
	if the given terms are not unifiable.

	Use Python dictionary to represent a subsititution map.
	'''
	def unify (self, t1 : Term, t2 : Term) -> dict:
 		pass

	def test_unify (self):
		t1 = Function("f", [Variable("X"), Variable("Y"), Variable ("Y")])
		t2 = Function("f", [Variable("Y"), Variable("Z"), Atom ("a")])
		u = { Variable("X"): Atom("a"), Variable("Y"): Atom("a"), Variable("Z"): Atom("a") }
		#assert
		(self.unify (t1, t2) == u)

	'''
	Problem 5
	Following the pseudocode Abstract interpreter in the lecture note Control in Prolog to implement
	a nondeterministic Prolog interpreter.

	nondet_query (program, goal) where
		the first argument is the program which is a list of Rules
		the second argument is the goal of type RuleBody (a conjunction of Functions/Terms).

	The function returns a list of Terms (results), which is an instance of the original goal and is
	a logical consequence of the program. See tests cases for expected results.
	'''
	def nondet_query (self, program : List[Rule], goal : RuleBody) -> RuleBody:
		pass

	# This test is different than above and that in OCaml in the sense that
	# 	(1) You can read a program from file:
	#	(2) You can input a query and see the result:
	def test_nondet_query(self, input_file):
		program_parser = PrologParser()
		self.request_parser = PrologParser('request')
		program = program_parser.parse(read(input_file))
		# You can print the input prolog program
		print (f'Print Input Program:')
		for rule in program:
			print (f'rule: {str(rule)}')

		while True:
			prefix = '?- '
			try:
				str_request = input(prefix)
			except KeyboardInterrupt:
				break
			if not str_request:
				break
			try:
				goal = self.request_parser.parse(prefix + str_request)
			except lark.exceptions.LarkError:
				print('Error : invalid syntax')
				continue
			try:
				# You can print the input goal
				print (f'Print Input Goal:')
				print (f'Query: {goal.body}')

				goal_ = self.nondet_query(program, goal)
				# You can print the solution goal_ here.
				print (f'Solution: {goal_}')
			except RecursionError:
				print('Error : stack overflow')
