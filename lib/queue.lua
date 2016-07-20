local queue = {}

function queue.new()
  return {first = 0, last = -1}
end

function queue.push(queue,value)
  local first = queue.first - 1
  queue.first = first
  queue[first] = value
end

function queue.pop(queue)
  local last = queue.last
  if queue.first > last then error("list is empty") end
  local value = queue[last]
  queue[last] = nil
  queue.last = last - 1
  return value
end

function queue.size(queue)
  return math.abs(queue.first - (queue.last + 1))
end

function queue.first(queue)
  return queue[queue.last]
end

return queue