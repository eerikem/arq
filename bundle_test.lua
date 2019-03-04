
local luaunit = require 'lib.luaunit'

colors = {}

local Bundle = require 'lib.bundle'

function test_bundle_new()
  local bundle = Bundle:new("right")
  local cable = bundle.white
  cable:enable()
end

function test_bundle_bad()
  local bundle = Bundle:new()
  bundle.white:enable()
end

function test_bundle_deprecated()
  local cable = Bundle:new("right","black")
  cable:enable()
end

function test_multiple_bundles()
  local bundle1 = Bundle:new("top")
  local bundle2 = Bundle:new("bottom")
  
  local blue = bundle1.blue
  local brown = bundle2.brown
  blue:isOn()
end

print("EXIT: "..luaunit.LuaUnit.run())