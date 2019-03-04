

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
-- @type Cable
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
local Cable={}

---
-- @type old
-- @extends #cable
-- @extends #Cable
-- @extends #BUNDLE

---
-- @type events
local e={badarg="Error: CABLE_SIDE required"}

---Global for backwords compatibility
-- @module BUNDLE
BUNDLE = {e=e}

---
-- @function [parent=#BUNDLE] new
-- @param self
-- @param #string CABLE_SIDE The side which the cable is connected
-- @usage bundle = Bundle:new("right")
-- @usage bundle.white:enable()
-- @return #Cable


local function newCable(Bundle,color,p)
  local cable = {cable=color,side=Bundle.side}
  setmetatable(cable,Cable)
  return cable
end

function BUNDLE:new(CABLE_SIDE,color,name)
  if not CABLE_SIDE then
    error(BUNDLE.e.badarg,2)
  end
  local bundle = {
    side=CABLE_SIDE,
    cable=color,
    name=name,
    flickering = false
  }
  setmetatable(bundle, self)
  
  if color then
    self.__index = Cable
  else
    local cable = {}
    cable.__index = function(self,key)
      if cable[key] == nil and colors[key] then
        cable[key]=newCable(self,key,name)
      end
      return cable[key]
    end
  setmetatable(bundle,cable)
  end
  return bundle
end

setmetatable(BUNDLE,Cable)
Cable.__index=Cable

---
-- @function [parent=#cable] enable
function Cable:enable()
  redstone.setBundledOutput(self.side,colors.combine(self.cable,redstone.getBundledOutput(self.side)))
end

---
-- @function [parent=#cable] disable
function Cable:disable()
  redstone.setBundledOutput(self.side,colors.subtract(redstone.getBundledOutput(self.side),self.cable))
end

---
-- @function [parent=#cable] isOn
function Cable:isOn()
  return colors.test(rs.getBundledInput(self.side),self.cable)
end

---
-- @function [parent=#old] isIn
function Cable:isIn()
  return colors.test(rs.getBundledInput(self.side),self.cable)
end

---
-- @function [parent=#old] isOut
function Cable:isOut()
  return colors.test(rs.getBundledOutput(self.side),self.cable)
end

---
-- @function [parent=#old] pulse
function Cable:pulse()
  self:enable()
  waitSeconds(1)
  self:disable()
end

---
-- @function [parent=#old] flick
function Cable:flick(low, high,low2,high2)
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
function Cable:flicker(low,high,low2,high2)
  if not low or not high then error("flicker must be given a low and high to limit random values",2) end
  if not low2 or not high2 then low2 = low high2 = high end
  if not self.flickering then
    self.flickering = true
    return runProcess(function() self:flick(low,high,low2,high2) end,"Flicker") 
  end
end

---
-- @function [parent=#old] stopFlicker
function Cable:stopFlicker()
  self.flickering = false
end

---
-- @function [parent=#old] getName
function Cable:getName()
  return self.name
end

return BUNDLE