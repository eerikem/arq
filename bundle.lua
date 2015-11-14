

colors.minus = function(colors,color)
  if bit32.band(colors,color)==color then
    return colors-color
  else
    return colors
  end
end

BUNDLE = {flickering = false}

function BUNDLE:new(o)
  --print "hello"
  local o = o or {}
  setmetatable(o, self)
  self.__index = self
  return o
end

function BUNDLE:new(_side,_cable,_name)
  local o = {
    side=_side,
    cable=_cable,
    name=_name
  }
  setmetatable(o, self)
  self.__index = self
  return o
end

function BUNDLE:enable()
  redstone.setBundledOutput(self.side,colors.combine(self.cable,redstone.getBundledOutput(self.side)))
end

function BUNDLE:disable()
  redstone.setBundledOutput(self.side,colors.minus(redstone.getBundledOutput(self.side),self.cable))
end

function BUNDLE:isOn()
  return colors.test(rs.getBundledInput(self.side),self.cable)
end

function BUNDLE:pulse()
  self:enable()
  waitSeconds(1)
  self:disable()
end

local function flick(low, high)
  while(flickering) do
    self.enable()
    waitSeconds(math.random(low,high))
    self.disable()
    waitSeconds(math.random(low,high))
  end
end

function BUNDLE:flicker(low,high,low2,high2)
  if not low or not high then error("flicker must be given a low and high to limit random values",2) end
  if not low2 or not high2 then low2 = low high2 = high end
  if not flickering then
    flickering = true
    return runProcess(flick(low2,high2))
  end
end

function BUNDLE:stopFlicker()
  flickering = false
end

function BUNDLE:getName()
  return self.name
end
