VM = require 'vm'

luaunit = require 'luaunit'

prod = require 'producer'
cons = require 'consumer'

function setup_each()
  VM.init()
end

function test_prod()
  local p = prod:new()
  local ran = false
  local cons = {}
  cons.handleEvent = function (e,...)
    local data = unpack(arg)
    luaunit.assertEquals(e,"event")
    ran = data
  end
  p:subscribe(cons)
  p:send("event",true)
  luaunit.assertTrue(ran)
  p:unsubscribe(cons)
  p:send("event",false)
  luaunit.assertTrue(ran)
end

function cons_test()
  local cons = consumer:new()
--  cons
  
  
end


os.exit(luaunit.LuaUnit.run())