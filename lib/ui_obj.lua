local Reactor = require 'reactor'
local Graphic = require 'graphic'
local panelIndex = 0
local protoIndex = 0
local Panel = {xpos=1,ypos=1,id="panel",height = 0,width = 0,absX = 0, absY= 0,noscroll = false}

local proto = {id="proto"}

function Panel:new()
  --content is a dictionary, index an ordered List
  local o = {content = {},index = {},layout="list"}
  o.reactor = Reactor:new(o)
  setmetatable(o, self)
  self.__index = self
  o.id="panel"..panelIndex
  panelIndex = panelIndex + 1
  o.proto = proto:new()
--  o:registerPanelHandlers()
  return o
end

function Panel:registerPanelHandlers()
  local handler = function(_,button,x,y)
    local pos = (self.ypos - 1)
    for _,o in ipairs(self.index) do
      pos = pos + o.ypos - 1
      local last = pos
      pos = pos + o.height
--      print("last: "..last.." pos: "..pos.." search: "..y)sleep(1)
      if y > last and y <= pos then
        return o.reactor:handleEvent("selected",button,x,y)
      end
    end
  end
  self.reactor:register("selected",handler)
end

function Panel:onMe(x,y)
  local indentX = self.xpos - 1
  local indentY = self.ypos - 1
  local width = self.width
  if width == "max" then
    width = 1000 --TODO better layout management
  end
  local height = self.height
  if self.staticHeight then
    height = self.staticHeight end
  if self.absY+ indentY <= y and y < self.absY + indentY + height then
    if x >= self.absX + indentX and x < self.absX + indentX + width then
      return true
    end
  end
  return false
end

Panel.setOnSelect= Graphic.setOnSelect

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
    if DEBUG then VM.log("Option 2: "..self.proto.background)stop()end
    out.setBackgroundColor(self.proto.background)
  elseif DEBUG then
    VM.log("Option 3: "..out.getBackgroundColor())
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
  if not self.proto then
    error(self.id,2)
  end
  local bp = self.proto.background
  local tp = self.proto.textColor
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
  
  if self.proto.backgroundFocus then
    out.setBackgroundColor(self.proto.backgroundFocus)
  end
  if self.proto.textFocus then
    out.setTextColor(self.proto.textFocus)
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
    counter = ui:printCentered(self.text,y,0,noscroll)
  else
    counter = ui:write(self.text,noscroll)
  end
  
  local x2,y2 = ui.term.getCursorPos()
  if y2-self.absY-(self.ypos-1) == 0 then
    self.width = x2-self.absX-(self.xpos - 1)
  else self.width = ui.term.getSize()
  end
  
  return counter
end

function proto:drawFromLine(ui,n,noscroll,focus)
  local color = ui.term.getTextColor()--TODO a better solution to Color bleeding.
  local back = ui.term.getBackgroundColor()
  if focus then
    self:colorFocus(ui.term)
  else
    self:color(ui.term)
  end
  local x = ui.term.getCursorPos()
  local w = ui.term.getSize()
  local tmp = self.text
  self.text = self:getTextFromLine(n,w-x+1)
  local counter = self:write(ui,noscroll)
  self.text = tmp
  self.height = counter + 1
  ui.term.setTextColor(color)
  ui.term.setBackgroundColor(back)
  return counter
end

function proto:redraw(ui,noscroll,focus)
  local color = ui.term.getTextColor()--TODO a better solution to Color bleeding.
  local back = ui.term.getBackgroundColor()
  if DEBUG then
    VM.log(string.format("At %d, %d",ui.term.getCursorPos()))
    VM.log(self.proto.id.." redraw before coloring text: "..color.." back: "..back)  
  end
  if focus then
    self:colorFocus(ui.term)
  else
    self:color(ui.term)
  end
  if DEBUG then
    VM.log(self.proto.id.." redraw after coloring: "..ui.term.getTextColor().." back: "..ui.term.getBackgroundColor())
--  ui.term.setBackgroundColor(colors.blue)
    ui.term.write(ui.term.getBackgroundColor())
  end
  local counter = self:write(ui,noscroll)
  if DEBUG then
    VM.log(self.proto.id.." written with text: "..ui.term.getTextColor().." back: "..ui.term.getBackgroundColor())
  end
  self.height = counter + 1
  ui.term.setTextColor(color)
  ui.term.setBackgroundColor(back)
  return counter
end

function proto:drawFocus(ui,noscroll)
  return self:redraw(ui,noscroll,true)
end

function proto:setCursor(ui)
  local x,y = ui.term.getCursorPos()
  self.absX = x
  self.absY = y
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

function Panel:getSize(width)
  local absWidth = 0
  local absHeight = 0
  local maxWidth = 0
  local xIndent = self.xpos - 1
  local yIndent = self.ypos - 1
  for _,V in ipairs(self.index) do
    local w,h = V:getSize(width-xIndent)
    if w > maxWidth then maxWidth = w end
    if self.layout == "static" then
      if h > absHeight then absHeight = h end
    else
      absHeight = absHeight + h
    end
  end
  return maxWidth + xIndent, absHeight + yIndent
end

function Panel:setLayout(layout)
  self.layout = layout
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
    c = c + self:drawItem(ui,self.index[start],true)
    start = start + 1
    num = num - 1
  end
  self.height = c
  return c
end

function Panel:drawFromLine(ui,n)
  self:applyColors(ui)
  local x,y = self:setCursor(ui)
  if self.width then
    local X,Y = ui.term.getCursorPos()
    local w = self.width
    if w == "max" then
      w = ui.term.getSize() end
--    print("Panel height: "..self.height) sleep(2)
    for n=1, self.height do
      for m=1, w do
        ui.term.write(" ")
      end
      incCursorPos(ui.term,x)
    end
    ui.term.setCursorPos(X,Y)
--    print("finished wiping panel")sleep(2)
  end
  --TODO generic panel setup for write.
  
  local _,height = ui.term.getSize()
  local first = true
  local limit = height - (y - 1)
  local sum = 0
  local i = 1
--  print(table.concat({height,limit,sum,i}," "))sleep(1)
  while ui.pane.index[i] and sum < limit do
    sum = sum + ui.pane.index[i].height
--    write("y: "..y.." sum: "..sum.." limit: "..limit)sleep(2)
    if sum >= n then
      if first then first = false
      else incCursorPos(ui.term,x) end
--      sum = sum + 1 end
      if sum - ui.pane.index[i].height + 1 >= n then
        sum = sum + self:drawItem(ui,ui.pane.index[i],true)
      else
        sum = sum + ui.pane.index[i]:drawFromLine(ui,n-(sum-ui.pane.index[i].height),true)
      end
    else
     limit = limit + ui.pane.index[i].height
    end
    i = i + 1
  end
end


Panel.setCursor = proto.setCursor

function Panel:redraw(ui,noscroll,focus)
  local color = ui.term.getTextColor()--TODO a better solution to Color bleeding.
  local back = ui.term.getBackgroundColor()
  self:applyColors(ui)
  local x,y = self:setCursor(ui)
  if self.width then
    local w = self.width
    if w == "max" then
      w = ui.term.getSize() end
    local h = self.height
    if self.staticHeight then
      h = self.staticHeight end
--    VM.log("Panel redraw max height: "..self.height)
    local first = true
    for n=1, h - (self.ypos - 1) do
      if first then first = false
      else incCursorPos(ui.term,x) end
      if DEBUG then
        VM.log(string.format("Drawing %d spaces from %d, %d",w,ui.term.getCursorPos()))
        end
      for m=1, w do
        ui.term.write(" ")
      end
    end
    ui.term.setCursorPos(x,y)
  end
  
  local function drawList()
    local first = true
    local lineCounter = 0
    for _,V in ipairs(self.index) do
      if first then first = false
      else incCursorPos(ui.term,x)
      lineCounter = lineCounter + 1 end
      lineCounter = lineCounter + self:drawItem(ui,V,noscroll,focus)
    end
    self.height = lineCounter + self.ypos
    ui.term.setTextColor(color)
    ui.term.setBackgroundColor(back)
    return lineCounter
  end
  
  local function drawStatic()
    local maxHeight = 0
    local maxWidth = 0
    local width,height = ui.term.getSize()
    for _,V in ipairs(self.index) do
      self:drawItem(ui,V,noscroll,focus)
      local w,h = V:getSize(width - (x - 1))
      if w > maxWidth then maxWidth = w end
      if h > maxHeight then maxHeight = h end
      ui.term.setCursorPos(x,y)
    end
    ui.term.setCursorPos(x,y + maxHeight - 1)
    self.height = maxHeight + self.ypos - 1
    if type(self.width) ~= "string" then
      self.width = maxWidth + self.xpos - 1
    end
    return maxHeight - 1 
  end
  
  if self.layout == "list" then
    return drawList()
  elseif self.layout == "static" then
    return drawStatic()
  else
    error("Unsupported layout "..self.layout)
  end
end

function Panel:drawFocus(ui,noscroll)
  return self:redraw(ui,noscroll,true)
end

function Panel:drawItem(ui,obj,noscroll,focus)
  return obj:redraw(ui,noscroll,focus)
end

function Panel:setHeight(height)
  self.staticHeight = height
end

function Panel:setContent(...)
  self.content = {}--TODO remove metatable?
  for _,V in ipairs(arg) do
    self:add(V)
  end
end

function Panel:remove(c)
--TODO shift index values in self.content?!?
  if self.content[c] then
    local pos = self.content[c]
    table.remove(self.index,pos)
    self.content[c]=nil
    for K,V in pairs(self.content) do
      if V > pos then
        self.content[K] = V - 1
      end
    end
    --TODO remove metatable!?
    if c.deleteProto then
--      c.deleteProto()
    end
    
    return pos
  end
  return nil
end

function Panel:insert(c,pos)
  table.insert(self.index,pos,c)
  self.content[c]=pos
  for i = pos + 1, table.maxn(self.index) do
    self.content[self.index[i]] = self.content[self.index[i]] + 1 
  end
end

function Panel:replace(c,_c)
  if not c or not _c then error("badarg",2) end
  local pos = self:remove(c)
  if pos then
    self:insert(_c,pos)
    self:applyProto(_c)
  else
    error("badarg, item not found",2)
  end
end

function Panel:applyProto(c)
  local deletionList = {}
  if not c.proto then
    c.proto = self.proto
    table.insert(deletionList,"proto")
    if not c.redraw then
      c.redraw = c.proto.redraw
      table.insert(deletionList,"redraw") end
    if not c.drawFocus then
      c.drawFocus = c.proto.drawFocus
      table.insert(deletionList,"drawFocus") end
    if not c.drawFromLine then
      c.drawFromLine = c.proto.drawFromLine
      table.insert(deletionList,"drawFocus") end
    c.color = c.proto.color
    c.colorFocus = c.proto.colorFocus
    c.setCursor = c.proto.setCursor
    if not c.write then
      c.write = c.proto.write
      table.insert(deletionList,"write") end
    c.deleteProto = function()
      for _,V in ipairs(deletionList) do
        c[V] = nil
      end
      c.deleteProto = nil
    end
  end
end

function Panel:add(c)
  if not c then error("obj expected",2) end
--  local o = {__index==c}
--  setmetatable(o,o)
  --local o = c:new()
  local o = c
  self:applyProto(o)
  table.insert(self.index,o)
  self.content[o]=table.maxn(self.index)
  return o
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
  self.height = counter
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