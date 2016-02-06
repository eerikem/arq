local UI = {}

function UI:new(term)
  if not term then error("UI needs a term",2) end
  --setmetatable(self,{__index = term})
  local o = {pane = Panel:new(),term = term}
  setmetatable(o,self)
  self.__index = self
  return o
end

function UI:draw(obj)
  obj:redraw(self)
end

function UI:add(obj)
  self.pane:add(obj)
  self:draw(obj)
end

function UI:printCentered(str, ypos)
  local w,h = self.term.getSize()
  --print("Printing '"..str.."' on "..self.name)
  if w >= string.len(str) then
    self.term.setCursorPos(w/2 - #str/2 + 1, ypos)
    self.term.write(str)
  else
    self:indentLeft(str,0,ypos)
    self:printCentered(string.sub(str,w+1),ypos+1)
  end
end

function UI:indentLeft(str, indent, ypos)
  self.term.setCursorPos(indent + 1, ypos)
  self.term.write(str)
end

function UI:setBackground(color)
  self.pane.background = color
end

function UI:setText(color)
  if type(self) == "number" then error("got it",2) end
  self.pane.textColor = color
end

function UI:update()
  local back = term.getBackgroundColor()
  self.term.setBackgroundColor(self.pane.background)
  self.term.clear()
  self.term.setCursorPos(1,1)
  self:draw(self.pane)
  term.setBackgroundColor(back)
end

return UI