
---
-- A generic event API for gen_server event tables.
-- Returns a table guarded against accidental nul indexes.
-- @module event
local Events = {}

Events.__index = function(table,key)
  if key == nil then
    error("badarg: attempt to index event table with nil",2)
  else
    error("Error, event table does not contain event "..key,2)
  end
end 

---
-- Guard a table against accidental nil indexes.
-- Causes the table to throw an error emmediately when a nil index is attempted.
-- @param self
-- @param #table e optional event table
-- @returns #table An empty table for defining event messages.
-- @usage Events:new(e) Apply metatable guard to existing table.
-- @usage local e = Events:new() Create a new table with guards already applied.
-- @usage e.someEvent = "event_msg" Add an event to the event table.
function Events:new(e)
  local events = e or {}
  setmetatable(events,self)
  return events
end

return Events