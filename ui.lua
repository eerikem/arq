--Product of ArqiTeknologies Corp.
--
--present
--
--Author: ArqiTek
--Copyright 2250

UI = {title = "ArqiTeknologies"}

function UI:new(o)
  --print "hello"
  local o = o or {}
  setmetatable(o, self)
  self.__index = self
  return o
end


function UI:wipe(n)
  local m = 0
  local x,y = self.getCursorPos()
  while m < n do
    self.write(" ")
    m = m + 1
  end
  self.setCursorPos(x,y)
end

function UI:writeChar(x,y,c)
  self.setCursorPos(x,y)
  self.write(c)
end

function UI:askUser(msg,x,y)
  if x == "center" then x = w/2 - msg.length/2 end
  if y == "center" then y = h/2 - 1 end
  
  local w = UI:new(window.create(self.term,x,y,w,4))
end

function UI:printCentered(str, ypos)
  local w,h = self.getSize()
  self.setCursorPos(w/2 - #str/2 + 1, ypos)
  self.write(str)
end

function UI:printRight(str, ypos)
  local w,h = self.getSize()
  self.setCursorPos(w - #str, ypos)
  self.write(str)
end

function UI:indentLeft(str, indent, ypos)
  self.setCursorPos(indent + 1, ypos)
  self.write(str)
end

function UI:terminate()
  self.setBackgroundColor(colors.black)
  self.clear()
  self.setCursorPos(1,1)
  self.write("thank you")
  sleep(1)
  self.clear()
  self.setCursorPos(1,1)
end


--MENUS
function UI:readMenu(menu)
  local items = {}
  local values = {}
  local selected = 1
  for i = 1, #menu, 2 do
    table.insert(items,menu[i])
    table.insert(values,menu[i+1])
  end
  
local function handleSelection(str, index, indent)
  if selected == index then
    self:indentLeft("["..str.."]", indent, index + 2 )
  else
    self:indentLeft(" "..str.."   ", indent, index + 2 )
  end
end


local function drawList(list)
  --self.list = list
  local l = table.getn(list)
  for i=1, l do
    handleSelection(list[i], i, 0)
  end
  --self:writeStatus(self.status)
end  
  
  local funs = {}
  funs.draw = function() drawList(items) end
  funs.run = function() runMenu() end
  return funs
end
--END MENUS





function UI:yesNo(str)
  local w,h = self.getSize()
  str = str .. " (Y/N)"
  local x = (w - #str) / 2 - 2
  local y = h/2 - 2
  local w = UI:new(window.create(self.current(),x,y,#str+4,3))
  local parent = self.redirect(w)
  w.setBackgroundColor(colors.blue)
  w.clear()
  w:printCentered(str,2)
  w.setBackgroundColor(colors.black)
  self.redirect(parent)
  
  while true do
    local id, K = os.pullEvent("key")
    local key = keys.getName(K)
    if key == "y" then
      return true
    elseif key == "n" then
      return false
    end
  end
end