local gen_server = require 'gen_server'

local Server = {}

function Server:new(o)
--  if not Pid then error("Producer expects Pid", 2) end
  local o = o or {}
  setmetatable(o,self)
  self.__index = self
  o.Pid = VM.running()
  o.subscribers = {}
  o.index = {}
  return o
end

local function addSub(server,sub)
  table.insert(server.subscribers,sub)
  server.index[sub]=#server.subscribers
end

local function remSub(server,sub)
  table.remove(server.subscribers,server.index[sub])
  server.index[sub] = nil
end

function Server:send(...)
  for _,cons in ipairs(self.subscribers) do
    cons.handleEvent(...)
  end
end

function Server:subscribe(cons)
  if not cons.handleEvent then error("Only Reactor Objects can subscribe.",2) end
  if not self.index[cons] then
    addSub(self,cons)
  end  
end

function Server:unsubscribe(cons)
  if self.index[cons] then
    remSub(self,cons)
  end
end

return Server