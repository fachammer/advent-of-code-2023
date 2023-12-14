package debug

extension [T](value: sourcecode.Text[T]) inline def d = pprint.log(value)
