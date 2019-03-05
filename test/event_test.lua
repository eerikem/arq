
local luaunit = require 'lib.luaunit'

local Events = require 'lib.event'

function test_new_event_table()
  local e = Events:new()
  luaunit.assertEquals(e,{})
end

function test_index_event_table()
  local e = Events:new()
  e.someEvent = "my_event"
  luaunit.assertEquals(e.someEvent,"my_event")
  luaunit.assertError(function() return e.someOtherEvent end)
  e.someEvent = nil
  luaunit.assertError(function() return e.someEvent end)
end

print("EXIT: "..luaunit.LuaUnit.run())