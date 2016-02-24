local panelIndex = 0
local protoIndex = 0
local Panel = {xpos=1,ypos=1,id="panel"}

local proto = {id="proto"}

function Panel:new()
  local o = {content = {},index = {}}
  setmetatable(o, self)
  self.__index = self
  o.id="panel"..panelIndex
  panelIndex = panelIndex + 1
  o.proto = proto:new()
  return o
end

function proto:new(o)
  local o = o or {}
  o.id="proto"..protoIndex
  protoIndex = protoIndex + 1
  setmetatable(o,self)
  self.__index = self
  return o
end

function proto:color(out)
  if self.background then
    out.setBackgroundColor(self.background)
  elseif self.proto.background then
    out.setBackgroundColor(self.proto.background)
  end
  if self.textColor then
    out.setTextColor(self.textColor)
  elseif self.proto.textColor then
    out.setTextColor(self.proto.textColor)
  end
end

function Panel:setBackgroundColor(c)
  self.proto.background = c
end

function Panel:setTextColor(c)
  self.proto.textColor = c
end

local function setColors(term,back,text)
  term.setBackgroundColor(back)
  term.setTextColor(text)
end

function proto:colorFocus(out)
  local b = self.background
  local t = self.textColor
  local bp = self.proto.background
  local tp = self.proto.background
  if b then
    if t then
      setColors(out,t,b)
    elseif tp then
      setColors(out,tp,b)
    else
      setColors(out,out.getTextColor(),b)
    end
  elseif bp then
    if t then
      setColors(out,t,bp)
    elseif tp then
      setColors(out,tp,bp)
    else
      setColors(out,out.getTextColor(),b)
    end
  else
    b = out.getBackgroundColor()
    if t then
      setColors(out,t,b)
    elseif tp then
      setColors(out,tp,b)
    else
      setColors(out,out.getTextColor(),b)
    end
  end
end

function proto:write(ui,noscroll)
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
    counter = ui:write(self.text,noscroll)
  end
  return counter
end

function proto:redraw(ui,noscroll,focus)
  local color = ui.term.getTextColor()--TODO a better solution to Color bleeding.
  local back = ui.term.getBackgroundColor()
  if focus then
    self:colorFocus(ui.term)
  else
    self:color(ui.term)
  end
  local counter = self:write(ui,noscroll)
  ui.term.setTextColor(color)
  ui.term.setBackgroundColor(back)
  return counter
end

function proto:drawFocus(ui,noscroll)
  return self:redraw(ui,noscroll,true)
end

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

function Panel:applyColors(ui)
  if self.proto.background then
    ui.term.setBackgroundColor(self.proto.background) end
  if self.proto.textColor then
    ui.term.setTextColor(self.proto.textColor) end
end

function Panel:drawSubset(ui,start,num)
  if start < 1 or start + num -1 > #self.index then
    error("bad indexes for subset",2) end
  local c = 0
  ui.pane:applyColors(ui)
  self:applyColors(ui)
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
  self:applyColors(ui)
  local x = self:setCursor(ui)
  local first = true
  local lineCounter = 0
  for _,V in ipairs(self.index) do
    if first then first = false
    else incCursorPos(ui.term,x)
    lineCounter = lineCounter + 1 end
    lineCounter = lineCounter + V:redraw(ui,noscroll)
  end
  ui.term.setTextColor(color)
  ui.term.setBackgroundColor(back)
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

function Panel:applyProto(c)
--  local m = getmetatable(c)
--  if m then self:applyProto(m)
--  else
--    setmetatable(c,{__index = self.proto})
--  end
  if not c.proto then
    c.proto = self.proto
    if not c.redraw then
    c.redraw = c.proto.redraw end
    c.drawFocus = c.proto.drawFocus
    c.color = c.proto.color
    c.colorFocus = c.proto.colorFocus
    c.setCursor = c.proto.setCursor
    c.write = c.proto.write
  end
--  local n = {__index = self.proto}
--  local m = getmetatable(c)
--  if m then
--    setmetatable(n,m)
--  end
--  setmetatable(c,n)
end

--function Panel:add(c)
--  self:applyProto(c)
--  table.insert(self.index,c)
--  self.content[c]=table.maxn(self.index)
--end

function Panel:add(c)
--  local o = {__index==c}
--  setmetatable(o,o)
  --local o = c:new()
  local o = c
  self:applyProto(o)
  table.insert(self.index,o)
  self.content[o]=table.maxn(self.index)
  return o
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
  self:applyColors(ui)
  local first = true
  local counter = 0
  for _,V in ipairs(self.index) do
    if first then first = false
    else incCursorPos(ui.term,x) counter = counter + 1 end
    counter = counter + V:redraw(ui,noscroll)
  end
  return counter
end

function List.fromArray(A)
  local l = List:new()
  for _,V in ipairs(A) do
    l:add(Graphic:new({text = V}))
  end
  return l
end

Panel.list = List
return Panel, List