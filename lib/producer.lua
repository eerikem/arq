local gen_server = require 'gen_server'

local Server = {}

---
-- @function [parent=#lib.producer] new
-- @param #producer self
-- @return lib.producer#producer
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
    gen_server.cast(cons,arg)
  end
end

function Server:subscribe(Co)
  if not Co then error("Badarg: Co expected",2) end
  if not self.index[Co] then
    addSub(self,Co)
  end  
end

function Server:unsubscribe(Co)
  if self.index[Co] then
    remSub(self,Co)
  end
end

return Server