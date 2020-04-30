from lark import Lark, Transformer, v_args
from lark.indenter import Indenter
from prolog_structures import Rule, RuleBody, Function, Variable, Atom, Number

class TreeToProlog(Transformer):
	program = list
	term_list = list

	@v_args(inline=True)
	def assertion(self, predicate):
		return Rule(predicate, RuleBody([]))
	@v_args(inline=True)
	def rule(self, predicate, body):
		return Rule(predicate, body)
	@v_args(inline=True)
	def request(self, body):
		return Rule(None, body)

	def conjunction(self, terms):
		return RuleBody(terms)

	@v_args(inline=True)
	def term(self, term1, relation, term2):
		return Function(relation, [term1, term2])
	sum_ = product = exponent = term

	@v_args(inline=True)
	def predicate(self, relation, terms=[]):
		return Function(relation, terms)
	@v_args(inline=True)
	def variable(self, value):
		return Variable(value.value)
	@v_args(inline=True)
	def atom(self, value):
		return Atom(value.value)
	main_op = sum_op = product_op = exponent_op = predicate_op = atom
	@v_args(inline=True)
	def number(self, value):
		return Number(value.value)

class PrologParser:
	def __init__(self, start = 'program'):
		self.lark_parser = Lark.open('src/prolog.lark', parser='lalr', \
			start=start, transformer=TreeToProlog())
	def parse(self, expression):
		return self.lark_parser.parse(expression)
