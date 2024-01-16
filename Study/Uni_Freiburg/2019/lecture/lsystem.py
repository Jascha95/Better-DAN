class LSystem:
    def __init__ (self, axiom, rules):
        self.axiom = axiom
        self.rules = rules

    def applyrules (self, w):
        rules = self.rules
        r = ""
        for c in w:
            r += rules.get(c,c)
        return r

    def generate (self, n : int):
        w = self.axiom
        for i in range(n):
            w = self.applyrules (w)
        return w

## returns expansion as an interator
class ILSystem (LSystem):
    def applyrules (self, w):
        rules = self.rules
        for c in w:
            for x in rules.get(c,c):
                yield x

