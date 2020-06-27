class Toggle {
  init(startState) {
    this.state = startState;
  }

  value() { return this.state; }

  activate() {
    this.state = !this.state;
    return this;
  }
}

class NthToggle < Toggle {
  init(startState, maxCounter) {
    super.init(startState);
    this.countMax = maxCounter;
    this.count = 0;
  }

  activate() {
    this.count = this.count + 1;
    if (this.count >= this.countMax) {
      super.activate();
      this.count = 0;
    }

    return this;
  }
}

let start = clock();
let n = 500;
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

toggle.value();

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

ntoggle.value();
