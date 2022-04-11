class Toggle
  def initialize(startState)
    @state = startState
  end

  def value
    @state
  end

  def activate
    @state = !@state
    self
  end
end

class NthToggle < Toggle
  def initialize(startState, maxCounter)
    super(startState)
    @countMax = maxCounter
    @count = 0
  end

  def activate
    @count += 1
    if @count >= @countMax
      super()
      @count = 0
    end

    self
  end
end

start = Time.new
n = 100000
val = true
toggle = Toggle.new(val)

n.times do |_|
  val = toggle.activate.value
  val = toggle.activate.value
  val = toggle.activate.value
  val = toggle.activate.value
  val = toggle.activate.value
  val = toggle.activate.value
  val = toggle.activate.value
  val = toggle.activate.value
  val = toggle.activate.value
  val = toggle.activate.value
end

puts toggle.value()

val = true
ntoggle = NthToggle.new(val, 3)

n.times do |_|
  val = ntoggle.activate.value
  val = ntoggle.activate.value
  val = ntoggle.activate.value
  val = ntoggle.activate.value
  val = ntoggle.activate.value
  val = ntoggle.activate.value
  val = ntoggle.activate.value
  val = ntoggle.activate.value
  val = ntoggle.activate.value
  val = ntoggle.activate.value
end

puts ntoggle.value
puts Time.new - start
