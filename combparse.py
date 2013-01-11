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


