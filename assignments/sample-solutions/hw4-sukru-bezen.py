import types
from functools import partial


class Universe():
    def __init__(self):
        self.mapping = {
            0: ["SUKRU", "ENIS", "UMUT", "GAMZE", "JANE", "MERIC", "JOHN"],
            1: ["SLEEPS", "WALKS", "THINKS"],
            2: ["TOUCHES", "LOVES", "HATES", "CHASES", "IS"]
        }

        for elem in self.mapping[1]:
            setattr(self, elem, self.one_arg_function_template)

        for elem in self.mapping[2]:
            setattr(self, elem, self.two_arg_function_template)

    def one_arg_function_template(self, a):
        return int(a in self.mapping[0])

    def two_arg_function_template(self, a, b):
        return self.one_arg_function_template(a) and self.one_arg_function_template(b)


pocket_universe = Universe()

def solve(alist):
    a, b = alist
    if type(a) == list:
        return partial(solve(a), b)
    elif type(a) == str:
        func = getattr(pocket_universe, a)

        return partial(func, b)

print solve(["SLEEPS", "MARY"])()
# 0

print solve(["SLEEPS", "UMUT"])()
# 1

print solve([["IS", "UMUT"], "UMUT"])()
# 1

print solve([["IS", "ENIS"], "SUKRU"])()
# 1