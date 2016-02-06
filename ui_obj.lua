local Panel = {xpos=1,ypos=1}

function Panel:new(o)
  local o = o or {content = {},index = {}}
  setmetatable(o, self)
  self.__index = self
  return o
end

local proto = {}

function proto:color(term)
  if(self.background) then
    term.setBackgroundColor(self.background) end
  if(self.textColor) then
    term.setTextColor(self.textColor) end
end

function proto:redraw(ui,noscroll)
  --error("here",2)
  local color = ui.term.getTextColor()--TODO a better solution to Color bleeding.
  local back = ui.term.getBackgroundColor()
  self:color(ui.term)
  local x,y = self:setCursor(ui)
  local w,h = ui.term.getSize()
  local counter = 0
  if y > h then
    if noscroll then
      return counter
    else
      ui.term.scroll((y+1)-h)
      ui.term.setCursorPos(x,h)
    end
  end
  if self.align == "center" then
    counter = ui:printCentered(self.text,y,noscroll)
  else
    counter = ui:indentLeft(self.text,0,y,noscroll)
  end
  ui.term.setTextColor(color)
  ui.term.setBackgroundColor(back)
  return counter
end

Panel.proto = proto

function proto:setCursor(ui)
  local x,y = ui.term.getCursorPos()
  x = x + self.xpos - 1
  y = y + self.ypos - 1
  ui.term.setCursorPos(x, y)
  return x, y
end

--Set the object's colors by assigning values to proto
function Panel:setColors(background,text)
  self.proto.background = background
  self.proto.textColor = text
end

function Panel:drawSubset(ui,start,num)
  if start < 1 or start + num -1 > #self.index then
    error("bad indexes for subset",2) end
  local c = 0
  local first = true
  while num > 0 do
    if first then first = false
    else incCursorPos(ui.term,1) c = c + 1 end
    c = c + self.index[start]:redraw(ui,true)
    start = start + 1
    num = num - 1
  end
  return c
end

Panel.setCursor = proto.setCursor

function Panel:redraw(ui,noscroll)
  local color = ui.term.getTextColor()--TODO a better solution to Color bleeding.
  local back = ui.term.getBackgroundColor()
  self:setColors(self.background,self.textColor)
  local x = self:setCursor(ui)
  local first = true
  local lineCounter = 0
  for _,V in ipairs(self.index) do
    if first then first = false
    else incCursorPos(ui.term,x)
    lineCounter = lineCounter + 1 end
    lineCounter = lineCounter + V:redraw(ui,noscroll)
  end
  term.setTextColor(color)
  term.setBackgroundColor(back)
  return lineCounter
end

function Panel:setContent(...)
  self.content = {}--TODO remove metatable?
  for _,V in ipairs(arg) do
    self:add(V)
  end
end

function Panel:remove(c)--TODO remove metatable?
  table.remove(c,self.content[c])
  self.content[c]=nil
end

local function applyProto(c)
  local m = getmetatable(c)
  if m then applyProto(m)
  else
    setmetatable(c,{__index = proto})
  end
end

function Panel:add(c)
  applyProto(c)
  table.insert(self.index,c)
  self.content[c]=table.maxn(self.index)
end


function Panel:redrawFocus()
  self:setColors(colors.gray,colors.lightGray)
  return self:draw()
end

local List = Panel:new()

function incCursorPos(term,xpos)
  local w,h = term.getSize()
  local x,y = term.getCursorPos()
  if xpos then x = xpos end
  if y + 1 <= h then
    term.setCursorPos(x,y+1)
  else
    term.scroll(1)
    term.setCursorPos(x,h)
  end
end

function List:redraw(ui,noscroll)
  local x = self:setCursor(ui)
  local first = true
  local counter = 0
  for _,V in ipairs(self.index) do
    if first then first = false
    else incCursorPos(ui.term,x) counter = counter + 1 end
    counter = counter + V:redraw(ui,noscroll)
  end
  return counter
end

function List:fromArray(A)
  local l = List:new()
  for _,V in ipairs(A) do
    l:add(Graphic:new({text = V}))
  end
  return l
end

return Panel, List