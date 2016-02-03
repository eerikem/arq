local Obj = {xpos=1,ypos=1}

function Obj:new(o)
  local o = o or {content = {}}
  setmetatable(o, self)
  self.__index = self
  return o
end

local proto = {}

function proto.write(text)
  term.write(text)
end

function proto:color()
  if(self.background) then
    term.setBackgroundColor(self.background) end
  if(self.textColor) then
    term.setTextColor(self.textColor) end
end

function proto:redraw()
  local color = term.getTextColor()
  local back = term.getBackgroundColor()
  self:color()
  self.write(self.text)
  term.setTextColor(color)
  term.setBackgroundColor(back)
end

Obj.proto = proto

--Set the object's colors by assigning values to proto
function Obj:setColors(background,text)
  self.proto.background = background
  self.proto.textColor = text
end

function Obj:draw()
  for K,_ in pairs(self.content) do
    K:redraw()
  end
end

function Obj:redraw()
  local color = term.getTextColor()
  local back = term.getBackgroundColor()
  self:setColors(self.background,self.textColor)
  self:draw()
  term.setTextColor(color)
  term.setBackgroundColor(back)
end

function Obj:setContent(...)
  self.content = {}--TODO remove metatable?
  for _,V in ipairs(arg) do
    self:addContent(V)
  end
end

function Obj:remove(c)--TODO remove metatable?
  self.content[c]=nil
end

function Obj:addContent(c)
  setmetatable(c,{__index=self.proto})
  self.content[c]=true
end


function Obj:redrawFocus()
  self:setColors(colors.gray,colors.lightGray)
  self:draw()
end

local List = Obj:new()

function List:setCursorPos()
  term.setCursorPos(self.xpos,self.ypos)
end

function List:incCursorPos()
  local _,y = term.getCursorPos()
  term.setCursorPos(self.xpos,y+1)
end

function List:draw()
  self:setCursorPos()
  for K,_ in pairs(self.content) do
    K:redraw()
    self:incCursorPos()
  end
end

function List:fromArray(A)
  local l = List:new()
  for _,V in ipairs(A) do
    l:addContent(Graphic:new({text = V}))
  end
  return l
end

return Obj, List