

colors.minus = function(colors,color)
  if bit32.band(colors,color)==color then
    return colors-color
  else
    return colors
  end
end

--Global for backwords compatibility
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
    name=_name,
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

function BUNDLE:flick(low, high,low2,high2)
  --local ui = UI:aquireAnyMonitor()
  --ui.clear()
  while self.flickering do
  -- term.redirect(ui)
  -- print("enable")
  --  term.redirect(term.native())
    self:enable()
    local x = math.random(low,high)
    waitSeconds(x+low2)
 --  term.redirect(ui)
 --   print("disable")
 --  term.redirect(term.native())
    self:disable()
    --local y = low + high * (1 - (high-low)/(x-low)
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