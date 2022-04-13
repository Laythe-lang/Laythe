class Foo
  def initialize
  end
end

start = Time.new

500000.times do |_|
  Foo.new
  Foo.new
  Foo.new
  Foo.new
  Foo.new
  Foo.new
  Foo.new
  Foo.new
  Foo.new
  Foo.new
  Foo.new
  Foo.new
  Foo.new
  Foo.new
  Foo.new
  Foo.new
  Foo.new
  Foo.new
  Foo.new
  Foo.new
  Foo.new
  Foo.new
  Foo.new
  Foo.new
  Foo.new
  Foo.new
  Foo.new
  Foo.new
  Foo.new
  Foo.new
end

puts Time.new - start

