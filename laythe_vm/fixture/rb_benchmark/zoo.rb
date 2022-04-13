class Zoo
  def initialize
    @aarletk  = 1
    @baboon   = 1
    @cat      = 1
    @donkey   = 1
    @elephant = 1
    @fox      = 1
  end

  def ant
    @aarletk
  end

  def banana
    @baboon
  end

  def tuna
    @cat
  end

  def hay
    @donkey
  end

  def grass
    @elephant
  end

  def mouse
    @fox
  end
end

zoo = Zoo.new
sum = 0
start = Time.new

while sum < 10000000 do
  sum += zoo.ant() + zoo.banana() + zoo.tuna() + zoo.hay() + zoo.grass() + zoo.mouse()
end

puts(sum)
puts(Time.new - start)
