import time

class Foo:
  def __init__(self):
    pass

start = time.time()

for _ in range(500000):
  Foo()
  Foo()
  Foo()
  Foo()
  Foo()
  Foo()
  Foo()
  Foo()
  Foo()
  Foo()
  Foo()
  Foo()
  Foo()
  Foo()
  Foo()
  Foo()
  Foo()
  Foo()
  Foo()
  Foo()
  Foo()
  Foo()
  Foo()
  Foo()
  Foo()
  Foo()
  Foo()
  Foo()
  Foo()
  Foo()

print(time.time() - start)

