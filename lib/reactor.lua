local Reactor = {run = true}

local reactorIndex = 0

--- Initialize new Reactor object
-- @function [parent=#lib.reactor] new
-- @param #lib.reactor self
-- @return #lib.reactor
function Reactor:new(parent)
  if not parent then error("badarg, no parent",2) end
  local o = {handlers = {},parent=parent}
  setmetatable(o,self)
  self.__index = self
  o.id = "reactor"..reactorIndex
  reactorIndex = reactorIndex + 1
  return o
end

function Reactor:stop() self.run = false end
function Reactor:start() self.run = true end

function Reactor:remove(h)
  if not h or not h.id then return end
  self.handlers[h.id] = nil
end

function Reactor:register(event,h)
  if not event and h then error("2 args required to register handler",2) end
--  print("registering "..event)
--  sleep(1)
  if not self.handlers[event] then
    self.handlers[event] = h
  else error("event: "..event.." already registered",2)
  end
end

function Reactor:handling(event)
  if self.handlers[event] then
    return true
  else
    return false
  end
end

function Reactor:handleEvent(...)
  if self.run then
    if arg[1] and self.handlers[arg[1]] then
      local handler = self.handlers[arg[1]]
      return handler(unpack(arg))
    else
      VM.log(self.id.." received unhandled event: "..arg[1],2)
    end
  end
end

function Reactor:handleReq(Req,State)
  if self.run then
    if Req[1] and self.handlers[Req[1]] then
      local handler = self.handlers[Req[1]]
      return handler(Req,State)
    else
      VM.log("reactor received unhandled event: "..Req[1],2)
    end
  end
end

return Reactor