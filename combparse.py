# The simplest, dumbest parser combinator library that actually kinda works

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
    if (data[pos:(len(text))] == text):
      return (True, data, pos + len(text), struct)
    return (False, data, pos, struct)
  return innerlit

def nop():
  def innernop(data, pos, struct):
    return (True, data, pos, struct)
  return innernop

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
      if (!cont):
        return (False, data, pos, struct)
    return (cont, newdata, newpos, newstruct)
  return innerseq

def many1(parser):
  return seq(parser, star(parser))

