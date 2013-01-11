# The simplest, dumbest parser combinator library that actually kinda works

import re

def alt(*parsers):
  def inneralt(data, pos, struct):
    for parser in parsers:
      (success, newdata, newpos, newstruct) = parser(data, pos, struct)
      if (success):
        return (True, newdata, newpos, newstruct)
    return (False, data, pos, struct)
  return inneralt

def lit(text):
  def innerlit(data, pos, struct):
    if (data[pos:(pos+len(text))] == text):
      return (True, data, pos + len(text), struct)
    return (False, data, pos, struct)
  return innerlit

def relit(regex):
  def innerrelit(data, pos, struct):
    pattern = re.compile(regex)
    match = pattern.match(data, pos)
    if (match):
      return (True, data, match.end(), struct)
    return (False, data, pos, struct)
  return innerrelit

def nop(data, pos, struct):
  return (True, data, pos, struct)


def opt(parser):
  return alt(parser, nop)

def star(parser):
  def innerstar(data, pos, struct):
    (loop, newdata, newpos, newstruct) = (True, data, pos, struct)
    while(loop):
      (loop, tempdata, temppos, tempstruct) = parser(newdata, newpos, newstruct)
      if (loop):
        (newdata, newpos, newstruct) = (tempdata, temppos, tempstruct)
    return (True, newdata, newpos, newstruct)
  return innerstar

def seq(*parsers):
  def innerseq(data, pos, struct):
    (cont, newdata, newpos, newstruct) = (True, data, pos, struct)
    for parser in parsers:
      (cont, newdata, newpos, newstruct) = parser(newdata, newpos, newstruct)
      if (not cont):
        return (False, data, pos, struct)
    return (cont, newdata, newpos, newstruct)
  return innerseq

def many1(parser):
  return seq(parser, star(parser))

def wrap(parser, func):
  def wrapped(data, pos, struct):
    (stasheddata, stashedpos, stashedstruct) = (data, pos, struct)
    (success, newdata, newpos, newstruct) = parser(data, pos, struct)
    if(success):
      (newdata, newpos, newstruct) = func(newdata, stashedpos, newpos, stashedstruct, newstruct)
    return (success, newdata, newpos, newstruct)
  return wrapped
