class Tree
  def initialize(depth)
    @depth = depth
    if depth > 0
      @a = Tree.new(depth - 1)
      @b = Tree.new(depth - 1)
      @c = Tree.new(depth - 1)
      @d = Tree.new(depth - 1)
      @e = Tree.new(depth - 1)
    end
  end

  def walk
    return 0 if @depth == 0

    return @depth + @a.walk() + @b.walk() + @c.walk() + @d.walk() + @e.walk()
  end
end
tree = Tree.new(8)

start = Time.new
for i in 0..100
  if tree.walk != 122068
    puts "Error"
  end
end

puts Time.new - start