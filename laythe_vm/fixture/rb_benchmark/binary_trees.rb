# self benchmark stresses both field and method lookup.
class Tree
  def initialize(item, depth)
    @item = item
    @depth = depth
    
    if depth > 0
      item2 = item + item;
      depth -= 1
      @right = Tree.new(item2 - 1, depth)
      @left = Tree.new(item2, depth)
    else
      @left = nil
      @right = nil
    end
  end

  def check
    return @item if @left.nil?

    @item + @left.check - @right.check  
  end
end

minDepth = 4
maxDepth = 14
stretchDepth = maxDepth + 1

start = Time.new

puts "stretch tree of depth:"
puts stretchDepth
puts "check:"
puts Tree.new(0, stretchDepth).check()

longLivedTree = Tree.new(0, maxDepth)

# iterations = 2 ** maxDepth
iterations = 1
d = 0
while d < maxDepth
  iterations *= 2
  d += 1
end

depth = minDepth
while depth < stretchDepth
  check = 0
  i = 1
  while i <= iterations
    check += Tree.new(i, depth).check + Tree.new(-i, depth).check
    i += 1
  end

  puts "num trees:"
  puts iterations * 2
  puts "depth:"
  puts depth
  puts "check:"
  puts check

  iterations /= 4
  depth += 2
end

puts "long lived tree of depth:"
puts maxDepth
puts "check:"
puts longLivedTree.check
puts "elapsed:"
puts Time.new - start
