

colors.minus = function(colors,color)
  if bit32.band(colors,color)==color then
    return colors-color
  else
    return colors
  end
end

---Global for backwords compatibility
-- @module Bundle
BUNDLE = {}

---
-- @param self
-- @param #string CABLE_SIDE The side which the cable is connected
-- @param #string color
-- @param #string name An optional name for the Bundle
-- @return #Bundle
function BUNDLE:new(CABLE_SIDE,color,name)
  
  local o = {
    side=CABLE_SIDE,
    cable=color,
    name=name,
    flickering = false
  }
  setmetatable(o, self)
  self.__index = self
  return o
end

function BUNDLE:enable()
  redstone.setBundledOutput(self.side,colors.combine(self.cable,redstone.getBundledOutput(self.side)))
end

function BUNDLE:disable()
  redstone.setBundledOutput(self.side,colors.subtract(redstone.getBundledOutput(self.side),self.cable))
end

--deprecated
function BUNDLE:isOn()
  return colors.test(rs.getBundledInput(self.side),self.cable)
end

function BUNDLE:isIn()
  return colors.test(rs.getBundledInput(self.side),self.cable)
end

function BUNDLE:isOut()
  return colors.test(rs.getBundledOutput(self.side),self.cable)
end

function BUNDLE:pulse()
  self:enable()
  waitSeconds(1)
  self:disable()
end

function BUNDLE:flick(low, high,low2,high2)
  while self.flickering do
    self:enable()
    local x = math.random(low,high)
    waitSeconds(x+low2)
    self:disable()
    waitSeconds(high2-x)
  end
end

function BUNDLE:flicker(low,high,low2,high2)
  if not low or not high then error("flicker must be given a low and high to limit random values",2) end
  if not low2 or not high2 then low2 = low high2 = high end
  if not self.flickering then
    self.flickering = true
    return runProcess(function() self:flick(low,high,low2,high2) end,"Flicker") 
  end
end

function BUNDLE:stopFlicker()
  self.flickering = false
end

function BUNDLE:getName()
  return self.name
end

return BUNDLE