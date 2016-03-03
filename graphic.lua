local Graphic = {text = "", xpos = 1, ypos = 1,height = 1,absX = 0,absY = 0, width = 0}

function Graphic:new(o)
  if type(o)=="string" then
  o = {text = o} end
  local o = o or {}
  setmetatable(o, self)
  self.__index = self
  o.reactor = Reactor:new()
  return o
end

function Graphic:setTextColor(c)
  self.textColor = c
end

function Graphic:setBackgroundColor(c)
  self.background = c
end

function Graphic:setOnSelect(ui,handler)
  self.reactor:register("selected",handler)
  ui:register(self,"selectable")
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