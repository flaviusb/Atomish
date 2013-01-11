# coding=utf-8

from combparse import *

Universe = {}

# AST nodes are dicts, created by functions

def atomish_comment(subtype):
  def inner_comment(text):
    return { 'type': "comment", 'value': { 'subtype': subtype, 'text': text } }
  return inner_comment

def atomish_decimal(num):
  return { 'type': 'decimal', 'value': float(num) }

def atomish_whole_number(num):
  return { 'type': 'integer', 'value': int(num) }

def atomish_operator(op):
  return { 'type': 'operator', value: op }

alpha = char_range("a", "z") + char_range("A", "Z")
ident = alpha + "_:?!→←⇒⇐$%" + char_range("0", "9")

# AST is a dict

# Mutually recursive functions have to be deffed, they cannot be simple vars

def limb(data, pos, struct):
  return alt(number, irony, comment, aside)

digit             = relit(r"[0-9]")
digits            = many1(digit)

decimal_number    = seq(opt(alt(lit("+"), lit("-"))), digits, lit("."), digits)
whole_number      = seq(opt(alt(lit("+"), lit("-"))), digits)
number            = alt(decimal_number, whole_number)

# Still uses pysec

expression_choices = []
expression        = choice(expression_choices)
decimal_number    = join_chars(group([option("", choice([match("-"), match("+")])), digits, match("."), digits])) >> Parser.lift(atomish_decimal)
whole_number      = join_chars(group([option("", choice([match("-"), match("+")])), digits])) >> Parser.lift(atomish_whole_number)
number            = choice([decimal_number, whole_number])
irony             = between(match('؟') , many_chars(none_of('\r\n')), ret) >> Parser.lift(atomish_comment('ironic'))
comment           = between(match('##') , many_chars(none_of('\r\n')), ret) >> Parser.lift(atomish_comment('earnest'))
aside             = between(match('#-') , many_chars(none_of('-#')), match('-#')) >> Parser.lift(atomish_comment('aside'))

name              = join_chars(group([one_of(alpha), many_chars(ident, min_count = 0)]))
operator          = many_chars('~!@$%^&*_=\'`/?×÷+-', min_count = 1) >> Parser.lift(atomish_operator)

interpolated      = group([match('#{')
expression_choices.extend([number, irony, comment, aside])
