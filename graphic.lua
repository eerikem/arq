local Graphic = {text = "", xpos = 1, ypos = 1}

function Graphic:new(o)
  if type(o)=="string" then
  o = {text = o} end
  local o = o or {}
  setmetatable(o, self)
  self.__index = self
  return o
end

--function Graphic:positionCursor()
--  term.setCursorPos(self.x,self.y)
--end

--function Graphic:write(text)
--  self.write(text)
--end


--function Graphic:redraw()
--  self:color()
--  self:positionCursor()
--  if self.text then
--    self:write(self.text)
--  end
--end

return Graphic