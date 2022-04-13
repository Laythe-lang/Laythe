import time

class Toggle:
  def __init__(self, startState):
    self.state = startState

  def value(self):
    return self.state

  def activate(self):
    self.state = not self.state
    return self

class NthToggle(Toggle):
  def __init__(self, startState, maxCounter):
    super().__init__(startState)
    self.countMax = maxCounter
    self.count = 0

  def activate(self):
    self.count += 1
    if self.count >= self.countMax:
      super().activate()
      self.count = 0

    return self

start = time.time()
n = 100000
val = True
toggle = Toggle(val)

for _ in range(n):
  val = toggle.activate().value()
  val = toggle.activate().value()
  val = toggle.activate().value()
  val = toggle.activate().value()
  val = toggle.activate().value()
  val = toggle.activate().value()
  val = toggle.activate().value()
  val = toggle.activate().value()
  val = toggle.activate().value()
  val = toggle.activate().value()

print(toggle.value())

val = True
ntoggle = NthToggle(val, 3)

for _ in range(n):
  val = ntoggle.activate().value()
  val = ntoggle.activate().value()
  val = ntoggle.activate().value()
  val = ntoggle.activate().value()
  val = ntoggle.activate().value()
  val = ntoggle.activate().value()
  val = ntoggle.activate().value()
  val = ntoggle.activate().value()
  val = ntoggle.activate().value()
  val = ntoggle.activate().value()

print(ntoggle.value())
print(time.time() - start)
