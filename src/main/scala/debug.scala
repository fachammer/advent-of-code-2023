package debug

def dbg[T](value: sourcecode.Text[T])(using
    enclosing: sourcecode.Enclosing
): T =
  pprint.log(value)
  value.value

extension [T](value: sourcecode.Text[T]) def d = dbg(value)
