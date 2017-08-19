
use = {}
defs = {}
succ = {}
nodes = []
ins = {}
outs = {}

instrs = [
  ['m <- 0', [], ['m'], []],
  ['v <- 0', [], ['v'], []],
  ['if v >= n goto 15', ['v', 'n'], [], [4, 15]],
  ['r <- v', ['v'], ['r'], []],
  ['s <- 0', [], ['s'], []],
  ['if r < n goto 9', ['r','n'], [], [9,7]],
  ['v <- v + 1', ['v'], ['v'], []],
  ['goto 3', [], [], [3]],
  ['x <- M[r]', ['r'], ['x'], []],
  ['s <- s + x', ['s','x'], ['s'], []],
  ['if s <= m goto 13', ['s', 'm'], [], [12, 13]],
  ['m <- s', ['s'], ['m'], []],
  ['r <- r + 1', ['r'], ['r'], []],
  ['goto 6', [], [], [6]],
  ['return m', ['m'], [], []],
]


instrs.each.with_index(1) do |instr, i|
  use[i], defs[i], succ[i] = instr[1..-1]
  ins[i], outs[i] = [], []
  nodes << i
end


loop do
  ins2 = {}
  outs2 = {}
  for i in nodes.reverse
    ins2[i] = ins[i].dup
    outs2[i] = outs[i].dup
    ins[i] = use[i] | (outs[i] - defs[i])
    succs = if i == nodes.last && succ[i] == []
              []
            elsif succ[i] != []
              succ[i]
            else
              [i+1]
            end
    for s in succs
      outs[i] |= ins[s]
    end
  end

  @i ||= 0
  @i += 1
  puts "Iteration #@i"
  puts "i\tin\tout"
  nodes.each do |i|
    puts "#{i}\t#{ins[i].join}\t#{outs[i].join}\t|\t#{instrs[i-1][0]}"
  end
  puts

  break if nodes.all? { |i| ins2[i] == ins[i] && outs2[i] == outs[i] }
end

