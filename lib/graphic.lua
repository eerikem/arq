local Reactor = require 'reactor'
local Graphic = {text = "", xpos = 1, ypos = 1,height = 1,absX = 0,absY = 0, width = 0}

function Graphic:new(o)
  if type(o)=="string" then
  o = {text = o,width = string.len(o)} end
  local o = o or {}
  setmetatable(o, self)
  self.__index = self
  o.reactor = Reactor:new(o)
  return o
end

function Graphic:setTextColor(c)
  self.textColor = c
end

function Graphic:setBackgroundColor(c)
  self.background = c
end

function Graphic:setOnSelect(ui,handler)
  self.reactor:register("mouse_up",handler)
  self.reactor:register("monitor_touch",handler)
  self.reactor:register("mouse_click",handler)
  ui:register(self,"clickable")
end

function Graphic:getSize(width)
  if not width then error("get size requires width for context") end
  local length = string.len(self.text)
  local w = self.xpos + length - 1
  local absWidth = 0
  local absHeight = 0
  if w > width then
    absWidth = width
    absHeight = math.ceil(length/width) + self.ypos - 1
  else
    absWidth = w
    absHeight = self.ypos
  end
  return absWidth, absHeight
end

function Graphic:getTextFromLine(line,width)
  if not line or not width or line < 1 then error("badargs",2) end
  local yIndent = self.ypos - 1
  local xIndent = self.xpos - 1
  local line = line - yIndent
  local length = string.len(self.text)
  width = width - xIndent
  if width * (line - 1) < length then
    local _,result = UI.lineWrap(self.text,width*(line - 1) + 1)
    return result
  else
    return nil
  end
end

function Graphic:onMe(x,y)
  local indentX = self.xpos - 1
  local indentY = self.ypos - 1
  local width = self.width
  if width == "max" then
    width = 1000 --TODO better layout management
  end
--  VM.log("Checking absY: "..self.absY.." and height: "..self.height)
  if self.absY+ indentY <= y and y < self.absY + indentY + self.height then
--    VM.log("X: "..x.." absX: "..self.absX.." width: "..width)
    if x >= self.absX + indentX and x < self.absX + indentX + width then
      return true
    end
  end
--  VM.log(x.." "..y.." not on graphic")
  return false
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