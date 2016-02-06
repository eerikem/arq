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

function proto:redraw(ui)
  --error("here",2)
  local color = ui.term.getTextColor()--TODO a better solution to Color bleeding.
  local back = ui.term.getBackgroundColor()
  self:color(ui.term)
  local x,y = self:setCursor(ui)
  if self.align == "center" then
    ui:printCentered(self.text,y)
  else
    ui.term.write(self.text)
  end
  ui.term.setTextColor(color)
  ui.term.setBackgroundColor(back)
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

Panel.setCursor = proto.setCursor

function Panel:redraw(ui)
  local color = ui.term.getTextColor()--TODO a better solution to Color bleeding.
  local back = ui.term.getBackgroundColor()
  self:setColors(self.background,self.textColor)
  local x = self:setCursor(ui)
  for K,_ in pairs(self.content) do
    K:redraw(ui)
    incCursorPos(ui.term,x)
  end
  term.setTextColor(color)
  term.setBackgroundColor(back)
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
  self:draw()
end

local List = Panel:new()

function incCursorPos(term,xpos)
  local x,y = term.getCursorPos()
  if xpos then x = xpos end
  term.setCursorPos(x,y+1)
end

function List:redraw(ui)
  local x = self:setCursor(ui)
  for _,V in ipairs(self.index) do
    V:redraw(ui)
    incCursorPos(ui.term,x)
  end
end

function List:fromArray(A)
  local l = List:new()
  for _,V in ipairs(A) do
    l:add(Graphic:new({text = V}))
  end
  return l
end

return Panel, List