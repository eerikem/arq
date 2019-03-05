
local luaunit = require 'lib.luaunit'

redstone = {}
rs = redstone
rs.getBundledInput = function() end
rs.getBundledOutput = function() end
rs.setBundledOutput = function() end

colors = {
    white="white",black="black",gray="gray",
    lightGray="lightGray", blue="blue",
    lightBlue="lightBlue", yellow="yellow",
    orange="orange",red="red",green="green",
    magenta="magenta",lime="lime",pink="pink",
    yan="cyan",purple="purple", brown="brown",    
    }
local rsBundle = {}

local function resetBundle()
  for k,v in pairs(colors) do
    rsBundle[k] = false
  end
end
    
colors.combine = function(color,_) rsBundle[color]=true end
colors.subtract = function(_,color) rsBundle[color]=false end
colors.test = function(_,color) return rsBundle[color] end

local Bundle = require 'lib.bundle'


function setup_each()
  resetBundle()
end

function test_bundle_new()
  local bundle = Bundle:new("right")
  local cable = bundle.white
  cable:enable()
  luaunit.assertTrue(rsBundle.white)
end

function test_no_cable_side_provided()
  luaunit.assertErrorMsgEquals(
    Bundle.e.badarg,Bundle.new,Bundle)
end

function test_bundle_deprecated()
  local cable = Bundle:new("right","black")
  cable:enable()
  luaunit.assertTrue(rsBundle.black)
end

function test_bundle_isOn()
  local bundle = Bundle:new("left")
  luaunit.assertFalse(bundle.orange:isOn())
  bundle.orange:enable()
  luaunit.assertTrue(bundle.orange:isOn())
end

function test_multiple_bundles()
  local bundle1 = Bundle:new("top")
  local bundle2 = Bundle:new("bottom")
  
  local blue = bundle1.blue
  local brown = bundle2.brown
  luaunit.assertNotEquals(blue.side,brown.side)
end

print("EXIT: "..luaunit.LuaUnit.run())