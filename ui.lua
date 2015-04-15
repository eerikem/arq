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
  --TODO integrate with CC OS timing events
  --waitSeconds(1)
  self.clear()
  self.setCursorPos(1,1)
end


function UI:showDelay(delay,h)
  local w = self.getSize() 
  for i=0, w do
    self:writeChar(1+i,h,'.')
    sleep(delay/w)
  end
end

function UI:writeChar(x,y,c)
  self.setCursorPos(x,y)
  self.write(c)
end

function UI:showDelayTwo(delay,h)
  local w = self.getSize()
  for i=0, w do
    self:writeChar(1+i,h,' ')
    sleep(delay/w)
    self:writeChar(1+i,h,'.')
  end
end

function UI:undoDelay(delay,h)
  local w = self.getSize()
  for i=0, w do
    self:writeChar(i,h,' ')
    sleep(delay/w)
  end
end


--MENUS
function UI:readMenu(menu)
  local items = {}
  local values = {}
  local menuIndent = 2
  local selected = 1
  local runMenu
  for i = 1, #menu, 2 do
    table.insert(items,menu[i])
    table.insert(values,menu[i+1])
  end
  
local function handleSelection(str, index, indent)
  if selected == index then
    self:indentLeft("["..str.."]", indent, index + menuIndent )
  else
    self:indentLeft(" "..str.."   ", indent, index + menuIndent )
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

local function handleEvent()
  local v = values[selected]
  if  type(v) == "function" then
    v(self)
  else
    local m = self:readMenu(v)
    m.run()
  end
end

local function setSelected(list,yPos)
  local menuItems = table.getn(list)
  if yPos - menuIndent <= menuItems and yPos - menuIndent > 0
  then selected = yPos - menuIndent
    return true
  else
    --self:writeStatus(yPos.." click")
    --print (yPos.." click")
    return false
  end
end

runMenu = function (list)
  --self:printCentered(root.title,1)
  self.clear()
  drawList(list)
  local running = true
  while running do
    local event, monitor, xPos, yPos = waitSignal("monitor_touch")
  --print(self.name.." received event from "..side)
  --os.pull Event picking up Event at the same time.
    if self.name == monitor and setSelected(list,yPos) then
      handleEvent(xPos,yPos)
    else
      runMenu(list)
    end
  end
end

  local funs = {}
  funs.draw = function() drawList(items) end
  funs.run = function() runMenu(items) end
  return funs
end
--END MENUS



function UI:aquireMonitor(_name)
  local mon = peripheral.find("monitor", function(name,object) return name == _name end)
  if mon then
    local ui = UI:new(mon)
    ui.name = _name
    return ui
  else
    error("Problem detecting " .. name)
  end
end


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
    local id, K = waitSignal("key")
    local key = keys.getName(K)
    if key == "y" then
      return true
    elseif key == "n" then
      return false
    end
  end
end