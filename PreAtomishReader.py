# coding=utf-8

from pysec import Parser, choice, quoted_chars, group_chars, option_chars, digits, between, pair, spaces, match, quoted_collection, quoted, many_chars, none_of, ret

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

# AST is a dict

expression_choices = []
expression        = choice(expression_choices)
decimal_number    = group_chars([choice([option_chars(["-"]), option_chars(["+"])]), digits, match("."), digits]) >> Parser.lift(atomish_decimal)
whole_number      = group_chars([choice([option_chars(["-"]), option_chars(["+"])]), digits]) >> Parser.lift(atomish_whole_number)
number            = choice([decimal_number, whole_number])
sarcastic_comment = between(match('؟') , many_chars(none_of('\r\n')), ret) >> Parser.lift(atomish_comment('sarcastic'))
expression_choices.extend([number, sarcastic_comment])
