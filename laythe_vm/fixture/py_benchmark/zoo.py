import time

class Zoo:
  def __init__(self):
    self.aarletk  = 1
    self.baboon   = 1
    self.cat      = 1
    self.donkey   = 1
    self.elephant = 1
    self.fox      = 1

  def ant(self):
    return self.aarletk

  def banana(self):
    return self.baboon

  def tuna(self):
    return self.cat

  def hay(self):
    return self.donkey

  def grass(self):
    return self.elephant

  def mouse(self):
    return self.fox

zoo = Zoo()
sum = 0
start = time.time()

while sum < 10000000:
  sum += (zoo.ant() \
      + zoo.banana() \
      + zoo.tuna() \
      + zoo.hay() \
      + zoo.grass() \
      + zoo.mouse())

print(sum)
print(time.time() - start)
