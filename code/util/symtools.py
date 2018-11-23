def _read_word(tape, word=""):
	if not tape or tape[0] in [' ','\t','(',')']:
		return word, tape 
	else:
		return _read_word(tape[1:], word + tape[0])

def _reduce_list(stack):
	store = []
	while stack and stack[-1] != '(':
		store = [stack.pop()] + store
	return stack[:-1] + [store]
		
def sym2py (symstring, stack=[]):
	"""
	>>> sym2py("(is (a ((mod chinese) pianist)))")
	['is', ['a', [['mod', 'chinese'], 'pianist']]]
	"""
	if not symstring:
		if len(stack) == 1:
			return stack[0]
		else:
			raise Exception("Ill-formed S-expr!")
	else:
		head = symstring[0]
		if head == '(':
			return sym2py(symstring[1:],stack + ['('])
		elif head in [' ','\t']:
			return sym2py(symstring[1:],stack)
		elif head == ')':
			return sym2py(symstring[1:],_reduce_list(stack))
		else:
			word, rest = _read_word(symstring)
			return sym2py(rest,stack + [word])
