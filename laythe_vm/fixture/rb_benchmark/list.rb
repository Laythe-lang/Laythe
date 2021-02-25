start = Time.new

10000.times do |i|
  collection = []
  1000.times do |j|
    collection.push(nil)
    collection[j] = j
  end
    
  sum = 0
  1000.times do |j|
    sum = sum + collection[1000 - j - 1]
    collection.pop
  end
end

puts Time.new - start