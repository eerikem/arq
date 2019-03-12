require 'cc_api'

local luaunit = require 'lib.luaunit'


function setup_each()
  resetBundle()
end

function test_arq_run()
  dofile('src/arq.lua')
end



print("EXIT: "..luaunit.LuaUnit.run())