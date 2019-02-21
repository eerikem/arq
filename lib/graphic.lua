local Reactor = require 'lib.reactor'
local Graphic = {text = "", xpos = 1, ypos = 1,height = 1,absX = 0,absY = 0, width = 0,lastClick=-1}


---@field [parent=#lib.graphic] lib.reactor#lib.reactor reactor

--- Initialize new Graphic object
-- @function [parent=#lib.graphic] new
-- @param #lib.graphic self
-- @param #string o Text to be displayed
-- @return lib.graphic#lib.graphic
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
  local function mouseHandler(type,id,button,x,y)
    if type == "mouse_click" then
      self.lastClick = id
    elseif type == "mouse_up" then
      if id == self.lastClick then
        self.reactor:handleEvent("selected")
      end
    end
  end
  self.reactor:register("mouse_up",mouseHandler)
  self.reactor:register("mouse_click",mouseHandler)
  self.reactor:register("monitor_touch",handler)
  self.reactor:register("selected",handler)
  ui:register(self,"clickable")
end

function Graphic:setJustOnSelect(ui,handler)
  self.reactor:register("selected",handler)
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

local function lineWrap(sText,w,line,room)
  local line = line or ""
  local room = room or w
  local whitespace = string.match( sText, "^[ \t]+" )
  local x = 1
  if whitespace then
    -- Print whitespace
    if string.len(whitespace) > room then
      line = line .. string.sub(whitespace,1,room)
      return line,string.sub(sText,room + 1)
    else
      x = string.len(whitespace) + 1
      line = line..whitespace
      sText = string.sub( sText, x )
    end
  end
  local newline = string.match( sText, "^\n" )
  if newline then
    return line .. "\n", string.sub( sText, 2 )
  end
  
  local text = string.match( sText, "^[^ \t\n]+" )
  if text then
    if string.len(text) > w then
      -- Print a multiline word      
        return line .. string.sub(text,1,room),string.sub( sText, room + 1 )
    else
      -- Print a word normally
      if string.len(line) + string.len(text) > w then
        return line, sText
      end
    sText = string.sub( sText, string.len(text) + 1 )
    return lineWrap(sText,w,line .. text,room - string.len(text))
    end
  end
  return line
end

function Graphic:getTextFromLine(line,width)
  if not line or not width or line < 1 then error("badargs",2) end
  local yIndent = self.ypos - 1
  local xIndent = self.xpos - 1
  local line = line - yIndent
  local length = string.len(self.text)
  width = width - xIndent
  if width * (line - 1) < length then
    local _,result = lineWrap(self.text,width*(line - 1) + 1)
    return result
  else
    return nil
  end
end

function Graphic:align(pos)
  self.myAlignment = pos
end

function Graphic:alignment()
  return self.myAlignment
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