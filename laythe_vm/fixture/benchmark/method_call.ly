class Toggle {
  init(startState) {
    self.state = startState;
  }

  value() { return self.state; }

  activate() {
    self.state = !self.state;
    return self;
  }
}

class NthToggle < Toggle {
  init(startState, maxCounter) {
    super.init(startState);
    self.countMax = maxCounter;
    self.count = 0;
  }

  activate() {
    self.count = self.count + 1;
    if (self.count >= self.countMax) {
      super.activate();
      self.count = 0;
    }

    return self;
  }
}

let start = clock();
let n = 1000000;
let val = true;
let toggle = Toggle(val);

for (let i = 0; i < n; i = i + 1) {
  val = toggle.activate().value();
  val = toggle.activate().value();
  val = toggle.activate().value();
  val = toggle.activate().value();
  val = toggle.activate().value();
  val = toggle.activate().value();
  val = toggle.activate().value();
  val = toggle.activate().value();
  val = toggle.activate().value();
  val = toggle.activate().value();
}

print toggle.value();

val = true;
let ntoggle = NthToggle(val, 3);

for (let i = 0; i < n; i = i + 1) {
  val = ntoggle.activate().value();
  val = ntoggle.activate().value();
  val = ntoggle.activate().value();
  val = ntoggle.activate().value();
  val = ntoggle.activate().value();
  val = ntoggle.activate().value();
  val = ntoggle.activate().value();
  val = ntoggle.activate().value();
  val = ntoggle.activate().value();
  val = ntoggle.activate().value();
}

print ntoggle.value();
print clock() - start;
