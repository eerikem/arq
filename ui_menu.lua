local Panel, List = require 'ui_obj'

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
  if self.width then
    local X,Y = ui.term.getCursorPos()
    local w = self.width
    if w == "max" then
      w = ui.term.getSize() end
    for n=1, self.height + self.ypos - 1 do
      for m=1, w do
        ui.term.write(" ")
      end
      incCursorPos(ui.term,X)
    end
    ui.term.setCursorPos(X,Y)
  end
  local x = self:setCursor(ui)
  
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
  self.width = maxWidth
  self.height = counter + 1
  return counter + self.ypos - 1
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

local function focusHandler(ui,menu)
  return function(event,button,x,y)
    if event == "monitor_touch" then
      y = x x = button button = nil 
      VM.log("Menu got touch")
    elseif button == 3 then
    --TODO monitorTouch seperate handler?
      return VM.log("Got button 3")
    end
    for _,obj in ipairs(menu.index) do
      if obj:onMe(x,y) then
        menu.focus = menu.content[obj]
        obj.reactor:handleEvent("focus")
        ui:update()
        if event == "monitor_touch" then
          return obj.reactor:handleEvent("selected")
        end
      end
    end
  end
end

local function mouseUpHandler(ui,menu)
  return function(_,button,x,y)
    if button == 3 then return end
    for _,obj in ipairs(menu.index) do
      if obj:onMe(x,y) then
        VM.log("Sending selected to menu item"..menu.content[obj])
        return obj.reactor:handleEvent("selected")
      end
    end
  end
end

local function scrollHandler(ui,menu)
  return function(_,direction)
    if direction == "scroll_up" then
      menu:dec()
      menu.index[menu.focus].reactor:handleEvent("focus")
    elseif direction == "scroll_down" then
      menu:inc()
      menu.index[menu.focus].reactor:handleEvent("focus")
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
      menu.index[menu.focus].reactor:handleEvent("focus")
      ui:update()
    elseif key == keys.down then
      menu:inc()
      menu.index[menu.focus].reactor:handleEvent("focus")
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
  self.reactor:register("scroll",scrollHandler(ui,self))
  --TODO overwriting reactor handlers?! automatic or not
--  self.reactor.handlers["selected"]=focusHandler(ui,self)
  self.reactor:register("mouse_click",focusHandler(ui,self))
  self.reactor:register("monitor_touch",focusHandler(ui,self))
  self.reactor:register("mouse_up",mouseUpHandler(ui,self))
  self.reactor:register("key",keyHandler(ui,self))
end

return Menu



