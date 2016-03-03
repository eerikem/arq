local Reactor = {run = true}

function Reactor:new()
  local o = {handlers = {}}
  setmetatable(o,self)
  self.__index = self
  return o
end

function Reactor:stop() self.run = false end
function Reactor:run() self.run = true end

function Reactor:remove(h)
  if not h or not h.id then return end
  self.handlers[h.id] = nil
end

function Reactor:register(event,h)
--  print("registering "..event)
--  sleep(1)
  if not self.handlers[event] then
    self.handlers[event] = h
  else error("event: "..event.." already registered",2)
  end
end

function Reactor:handleEvent(...)
  if arg[1] and self.handlers[arg[1]] then
    local handler = self.handlers[arg[1]]
    return handler(unpack(arg))
  else
    VM.log("reactor received unhandled event: "..arg[1],2)
  end
end

function Reactor:handleReq(Req,State)
  if Req[1] and self.handlers[Req[1]] then
    local handler = self.handlers[Req[1]]
    return handler(Req,State)
  else
    VM.log("reactor received unhandled event: "..Req[1],2)
  end
end

return Reactor