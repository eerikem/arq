local Panel, List = require 'lib.ui_obj'

local menuIndex = 0

local Menu = Panel:new()
Menu.id = "menu"
Menu.focus = 1
Menu.listeners = {"key","mouse_click","monitor_touch"}
Menu.handlers = {keyHandler}--TODO fix this?!?


function Menu.fromList(list)
  menuIndex = menuIndex + 1
  local m = Menu:new()
  m.id = "menu"..menuIndex
  for _,V in ipairs(list.index) do
    m:add(V)
    --todo, hard overwrite of proto side effects?!?
    V.proto = m.proto
  end
  m.ypos = list.ypos
  m.xpos = list.xpos
  return m
end

function Menu.inlineOnFocus(obj,subObj)
  obj.drawFocus = function (graphic,ui,...)
    local n = 1
    local x = ui.term.getCursorPos()
    --TODO passing graphic to proto.drawFocus side effects!?!?
    n = n + graphic.proto.drawFocus(graphic,ui,arg)
    incCursorPos(ui.term,x)
    n = n + ui:draw(subObj)
    return n
  end
end

function Menu.fromArray(array)
  return Menu.fromList(List.fromArray(array))
end

function Menu:redraw(ui,noscroll)
  local color = ui.term.getTextColor()--TODO a better solution to Color bleeding.
  local back = ui.term.getBackgroundColor()
  self:applyColors(ui)
  local x,y = self:setCursor(ui)
  local X = x
  if self.width then
    local w = self.width
    if w == "max" then
      x = 1
      ui.term.setCursorPos(x,y)
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
    x = X
    ui.term.setCursorPos(x,y)
  end
  
  local first = true
  local counter = 0
  local maxWidth = 0
  for n,V in ipairs(self.index) do
    if first then first = false
    else incCursorPos(ui.term,x) counter = counter + 1 end
    if n == self.focus then
      counter = counter + V:drawFocus(ui,noscroll)
    else
      counter = counter + V:redraw(ui,noscroll)
    end
    if V.width > maxWidth then
      maxWidth = V.width end
--    write("count = "..counter)sleep(1)
  end
  if self.width ~= "max" then
  self.width = maxWidth end
  self.height = counter + self.ypos
  ui.term.setTextColor(color)
  ui.term.setBackgroundColor(back)
  return counter
end

function Menu:inc()
  if self.focus < #self.index then
    self.focus = self.focus + 1
  else
    self.focus = 1
  end
end

function Menu:dec()
  if self.focus > 1 then
    self.focus = self.focus - 1
  else
    self.focus = #self.index
  end
end

local function sendFocus(obj)
  if obj.reactor:handling("focus") then
    obj.reactor:handleEvent("focus")
  end
end

local function focusHandler(ui,menu)
  return function(event,button,x,y)
--    VM.log("Menu got mouse click")
    if event == "monitor_touch" then
      y = x x = button button = nil 
--      VM.log("Menu got touch")
    elseif button == 3 then
    --TODO monitorTouch seperate handler?
      return VM.log("Got button 3")
    end
    for _,obj in ipairs(menu.index) do
      if obj:onMe(x,y) then
        menu.focus = menu.content[obj]
        sendFocus(obj)
        ui:update()
        if event == "monitor_touch" then
          return obj.reactor:handleEvent("selected")
        end
      end
    end
  end
end

--TODO optimize drag handling for menu
local function dragHandler(ui,menu)
  return function(_,click,button,x,y)
    x,y = ui:relativeXY(x,y)
    for _,obj in ipairs(menu.index) do
      if obj:onMe(x,y) then
        menu.focus = menu.content[obj]
        sendFocus(obj)
        ui:update()
      end
    end
  end
end

local function mouseUpHandler(ui,menu)
  return function(_,button,x,y)
--    VM.log("Menu got mouse up")
    if button == 3 then return end
    for _,obj in ipairs(menu.index) do
      if obj:onMe(x,y) then
--        VM.log("Sending selected to menu item"..menu.content[obj])
        return obj.reactor:handleEvent("selected")
      end
    end
  end
end

local function scrollHandler(ui,menu)
  return function(_,direction)
    if direction == "scroll_up" then
      menu:dec()
      sendFocus(menu.index[menu.focus])
    elseif direction == "scroll_down" then
      menu:inc()
      sendFocus(menu.index[menu.focus])
    else
      error("bad scroll event")
    end
    ui:update()
  end
end

local function keyHandler(ui,menu)
  return function(_,key)
    if key == keys.up then
      menu:dec()
      sendFocus(menu.index[menu.focus])
      ui:update()
    elseif key == keys.down then
      menu:inc()
      sendFocus(menu.index[menu.focus])
      ui:update()
    elseif key == keys.enter then
      return menu.index[menu.focus].reactor:handleEvent("selected")
    else
      VM.log("Menu not handling "..keys.getName(key))
    end
  end
end

function Menu:link(ui)--TODO add link to all objects and call automaticaly in add?
  ui:register(self,"clickable")
  --self.reactor:register("selected",itemSelected)
  ui:register(self,"keys")
  ui:register(self,"scroll")
  ui:register(dragHandler(ui,self),"draggable")
  self.reactor:register("scroll",scrollHandler(ui,self))
  --TODO overwriting reactor handlers?! automatic or not
--  self.reactor.handlers["selected"]=focusHandler(ui,self)
  self.reactor:register("mouse_click",focusHandler(ui,self))
  self.reactor:register("monitor_touch",focusHandler(ui,self))
  self.reactor:register("mouse_up",mouseUpHandler(ui,self))
  self.reactor:register("key",keyHandler(ui,self))
end

return Menu



