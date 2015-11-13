

colors.minus = function(colors,color)
  if bit32.band(colors,color)==color then
    return colors-color
  else
    return colors
  end
end

BUNDLE = {}

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

function BUNDLE:getName()
  return self.name
end