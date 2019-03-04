

colors.minus = function(colors,color)
  if bit32.band(colors,color)==color then
    return colors-color
  else
    return colors
  end
end

---
-- @type cable

---
-- @type Bundle
-- @field #cable white
-- @field #cable black
-- @field #cable gray
-- @field #cable lightGray
-- @field #cable blue
-- @field #cable lightBlue
-- @field #cable yellow
-- @field #cable orange
-- @field #cable red
-- @field #cable green
-- @field #cable magenta
-- @field #cable lime
-- @field #cable pink
-- @field #cable cyan
-- @field #cable purple
-- @field #cable brown 
local Bundle={}

---
-- @type old
-- @extends #cable
-- @extends #Bundle
-- @extends #BUNDLE

---Global for backwords compatibility
-- @module BUNDLE
BUNDLE = {}

local colors = {
    white=1,black=1,gray=1,lightGray=1,
    blue=1,lightBlue=1,yellow=1,orange=1,
    red=1,green=1,magenta=1,lime=1,pink=1,
    cyan=1,pruple=1,brown=1,    
    }

---
-- @function [parent=#BUNDLE] new
-- @param self
-- @param #string CABLE_SIDE The side which the cable is connected
-- @usage bundle = Bundle:new("right")
-- @usage bundle.white:enable()
-- @return #Bundle


local function newCable(b,color,p)
  local o = {cable=color,side=b.side}
  setmetatable(o,Bundle)
  return o
end

function BUNDLE:new(CABLE_SIDE,color,name)
  if not CABLE_SIDE then
    error("Error: CABLE_SIDE required",2)
  end
  local o = {
    side=CABLE_SIDE,
    cable=color,
    name=name,
    flickering = false
  }
  setmetatable(o, self)
  
  if color then
    self.__index = Bundle
  else
    local b = {}
    b.__index = function(self,key)
      if b[key] == nil and colors[key] then
        b[key]=newCable(self,key,name)
      else
        return Bundle[key]
      end
      return b[key]
    end
  setmetatable(o,b)
  end
  return o
end

setmetatable(BUNDLE,Bundle)
Bundle.__index=Bundle

---
-- @function [parent=#cable] enable
function Bundle:enable()
  redstone.setBundledOutput(self.side,colors.combine(self.cable,redstone.getBundledOutput(self.side)))
end

---
-- @function [parent=#cable] disable
function Bundle:disable()
  redstone.setBundledOutput(self.side,colors.subtract(redstone.getBundledOutput(self.side),self.cable))
end

---
-- @function [parent=#cable] isOn
function Bundle:isOn()
  return colors.test(rs.getBundledInput(self.side),self.cable)
end

---
-- @function [parent=#old] isIn
function Bundle:isIn()
  return colors.test(rs.getBundledInput(self.side),self.cable)
end

---
-- @function [parent=#old] isOut
function Bundle:isOut()
  return colors.test(rs.getBundledOutput(self.side),self.cable)
end

---
-- @function [parent=#old] pulse
function Bundle:pulse()
  self:enable()
  waitSeconds(1)
  self:disable()
end

---
-- @function [parent=#old] flick
function Bundle:flick(low, high,low2,high2)
  while self.flickering do
    self:enable()
    local x = math.random(low,high)
    waitSeconds(x+low2)
    self:disable()
    waitSeconds(high2-x)
  end
end

---
-- @function [parent=#old] flicker
function Bundle:flicker(low,high,low2,high2)
  if not low or not high then error("flicker must be given a low and high to limit random values",2) end
  if not low2 or not high2 then low2 = low high2 = high end
  if not self.flickering then
    self.flickering = true
    return runProcess(function() self:flick(low,high,low2,high2) end,"Flicker") 
  end
end

---
-- @function [parent=#old] stopFlicker
function Bundle:stopFlicker()
  self.flickering = false
end

---
-- @function [parent=#old] getName
function Bundle:getName()
  return self.name
end

return BUNDLE