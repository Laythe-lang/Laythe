# self benchmark stresses both field and method lookup.
class Foo
  def initialize()
    @field0 = 1
    @field1 = 1
    @field2 = 1
    @field3 = 1
    @field4 = 1
    @field5 = 1
    @field6 = 1
    @field7 = 1
    @field8 = 1
    @field9 = 1
    @field10 = 1
    @field11 = 1
    @field12 = 1
    @field13 = 1
    @field14 = 1
    @field15 = 1
    @field16 = 1
    @field17 = 1
    @field18 = 1
    @field19 = 1
    @field20 = 1
    @field21 = 1
    @field22 = 1
    @field23 = 1
    @field24 = 1
    @field25 = 1
    @field26 = 1
    @field27 = 1
    @field28 = 1
    @field29 = 1
  end

  def method0
    @field0
  end

  def method1
    @field1
  end

  def method2
    @field2
  end

  def method3
    @field3
  end

  def method4
    @field4
  end

  def method5
    @field5
  end

  def method6
    @field6
  end

  def method7
    @field7
  end

  def method8
    @field8
  end

  def method9
    @field9
  end

  def method10
    @field10
  end

  def method11
    @field11
  end

  def method12
    @field12
  end

  def method13
    @field13
  end

  def method14
    @field14
  end

  def method15
    @field15
  end

  def method16
    @field16
  end

  def method17
    @field17
  end

  def method18
    @field18
  end

  def method19
    @field19
  end

  def method20
    @field20
  end

  def method21
    @field21
  end

  def method22
    @field22
  end

  def method23
    @field23
  end

  def method24
    @field24
  end

  def method25
    @field25
  end

  def method26
    @field26
  end

  def method27
    @field27
  end

  def method28
    @field28
  end

  def method29
    @field29
  end
end

foo = Foo.new
start = Time.new
for i in 0..500000
  foo.method0()
  foo.method1()
  foo.method2()
  foo.method3()
  foo.method4()
  foo.method5()
  foo.method6()
  foo.method7()
  foo.method8()
  foo.method9()
  foo.method10()
  foo.method11()
  foo.method12()
  foo.method13()
  foo.method14()
  foo.method15()
  foo.method16()
  foo.method17()
  foo.method18()
  foo.method19()
  foo.method20()
  foo.method21()
  foo.method22()
  foo.method23()
  foo.method24()
  foo.method25()
  foo.method26()
  foo.method27()
  foo.method28()
  foo.method29()
end

puts Time.new - start