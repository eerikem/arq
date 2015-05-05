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


function UI:wipe(n,x,y)
  local m = 0
  local a,b = self.getCursorPos()
  self.setCursorPos(x,y)
  while m < n do
    self.write(" ")
    m = m + 1
  end
  self.setCursorPos(a,b)
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
  waitSeconds(1)
  self.clear()
  self.setCursorPos(1,1)
end


function UI:showDelay(delay,h)
  local w = self.getSize() 
  for i=0, w do
    self:writeChar(1+i,h,'.')
    waitSeconds(delay/w)
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
    waitSeconds(delay/w)
    self:writeChar(1+i,h,'.')
  end
end

function UI:undoDelay(delay,h)
  local w = self.getSize()
  for i=0, w do
    self:writeChar(i,h,' ')
    waitSeconds(delay/w)
  end
end

function UI:read()
  self.setCursorBlink(true)
  local _x,_y = self.getCursorPos()
  local buffer = {}
  local n = 1
  waitAny()
  local event, key = coroutine.yield()
  self.setCursorBlink(false)
  --print("received: "..event)
  while true do
    if event == "key" then
      key = keys.getName(key)
      if key == "enter" then
        self.setCursorBlink(false)
        local r = buffer[1]
        for i=2, n-1 do
          r = r .. buffer[i]
        end
        writeStatus("Read: " .. r)
        stopWait()
        return r
      elseif key == "backspace" then
        local x, y = self.getCursorPos()
        if x > _x then
          x = x - 1
          self.setCursorPos(x,y)
          self.write(" ")
          self.setCursorPos(x,y)
          n = n - 1
          buffer[n]=nil
        end
      end
    end
    if event == "char" then
      buffer[n] = key
      n = n + 1
      self.write(key)
    end
    self.setCursorBlink(true)
    event, key = coroutine.yield()
  end
end


--MENUS
function UI:readMenu(menu)
  local items = {}
  local values = {}
  local menuIndent = 2
  local menuLeftIndent = 0
  local selected = 1
  local runMenu
  local title
  local running = true
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
  if title ~= nil then 
  self:indentLeft(title,0,1) end
  local l = table.getn(list)
  for i=1, l do
    if type(list[i]) == "function" then
      handleSelection(list[i](), i, menuLeftIndent)
    else
      handleSelection(list[i], i, menuLeftIndent)
    end
  end
end

local function handleSelection()
  local yPos = selected + menuIndent
  local v = values[selected]
  if type(v) == "function" then
    local msg = v(self)
    --Return msg used to control menu
    if msg == "kill" then running = false
    return false end
  elseif v == "BACK" then
    running = false
  elseif type(v) == "table" then
    local m = self:readMenu(v)
    m.setTitle(items[selected])
    m.cycle()
  else
    self:wipe(#items[selected]+2,menuLeftIndent+1,yPos)
    self:indentLeft("Bad Item",menuLeftIndent+1,yPos)
    waitSeconds(1.5)
  end
  return true
end

local function setSelected(list,yPos)
  local menuItems = table.getn(list)
  if yPos - menuIndent <= menuItems and yPos - menuIndent > 0
  then selected = yPos - menuIndent
    drawList(list)
    return true
  else
    --self:writeStatus(yPos.." click")
    print (yPos.." click")
    return false
  end
end

cycleMenu = function (list)
  while running do
    runMenu(list)
  end
end

local runMenuMonitor = function (list)
  self.clear()
  drawList(list)
  local event, monitor, xPos, yPos = waitSignal("monitor_touch")
  if event == "terminate" then self.clear() return false end
  if self.name == monitor and setSelected(list,yPos) then
    return handleSelection()
  else
    return runMenu(list)
  end
end
  
runMenu = function (list)
  if self.name ~= nil then
    runMenuMonitor(list)
  else
    drawList(list)
    --printTable(getWaitList())
    local co1 = runProcess(function()
    local event, button, x, y
    while true do 
      event, button, x,y = waitSignal("mouse_click")
      if event == "terminate" then break end
      if setSelected(list,y) then
        signal("list_selected")
        break;
      end
      end
    end
      , "Menu_Mouse_Lstnr")
    local co2 = runProcess(function()
    local event, key
    while true do
      event, key = waitSignal("key")
      if event == "terminate" then break end
      key = keys.getName(key)
      if key == "up" then
        if selected ~= 1 then
          selected = selected - 1
          drawList(list)
        end
      elseif key == "down" then
        if selected ~= #items then
          selected = selected + 1
          drawList(list)
        end
      elseif key == "enter" then
        signal("list_selected")
        break;
      end
    end
    end
    , "Menu_Key_Lstnr")
    
    waitSignal("list_selected")
    stop(co1)
    stop(co2)
    return handleSelection()
  end
end

  local funs = {}
  funs.draw = function() drawList(items) end
  funs.run = function() runMenu(items) end
  funs.cycle = function() cycleMenu(items) end
  funs.setTitle = function(_t) title = _t end 
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
  parent.setBackgroundColor(colors.black)
  w.clear()
  w:printCentered(str,2)
  w.setBackgroundColor(colors.black)
  --w:printCentered("hello?",3)
  self.redirect(parent)
  for k,v in pairs(w) do
   -- if k == "setBackgroundColor" then parent:indentLeft(tostring(v),0,4) end
  --term.write(tostring(k).."  ")
  end
  for k,v in pairs(parent) do
    --if k == "setBackgroundColor" then parent:indentLeft(tostring(v),0,5) end
  end
  
  while true do
    local id, K = waitSignal("key")
    local key = keys.getName(K)
    if key == "y" then
      w.clear()
      return true
    elseif key == "n" then
      w.clear()
      return false
    end
  end
end