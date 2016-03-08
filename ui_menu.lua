local Panel, List = require 'ui_obj'

local menuIndex = 0

local Menu = Panel:new()
Menu.id = "menu"
Menu.selected = 1
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
    --todo passing graphic to proto.drawFocus side effects!?!?
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
  local x = self:setCursor(ui)
--  print("menu set cursor Pos "..x)sleep(3)
  self:applyColors(ui)
  local first = true
  local counter = 0
  local maxWidth = 0
  for n,V in ipairs(self.index) do
    if first then first = false
    else incCursorPos(ui.term,x) counter = counter + 1 end
    if n == self.selected then
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
  return counter
end

function Menu:inc()
  if self.selected < #self.index then
    self.selected = self.selected + 1
  else
    self.selected = 1
  end
end

function Menu:dec()
  if self.selected > 1 then
    self.selected = self.selected - 1
  else
    self.selected = #self.index
  end
end

local function selectedHandler(ui,menu)
  return function(_,button,x,y)
    for _,obj in ipairs(menu.index) do
      --TODO make this generic operation
      if obj.absY <= y and y < obj.absY + obj.height then
        print("Changing selected to "..menu.content[obj])sleep(1)
        menu.selected = menu.content[obj]
        ui:update()
        return obj.reactor:handleEvent("selected")
      end
    end
  end
end

local function scrollHandler(ui,menu)
  return function(_,direction)
    if direction == "scroll_up" then
      menu:dec()
    elseif direction == "scroll_down" then
      menu:inc()
    else
      error("bad scroll event")
    end
    ui:update()
  end
end

local function itemSelected(ui,menu)
  return function(_,button,x,y)
    
  end
end

function Menu:link(ui)
  ui:register(self,"clickable")
  --self.reactor:register("selected",itemSelected)
  ui:register(self,"keys")
  ui:register(self,"scroll")
  self.reactor:register("scroll",scrollHandler(ui,self))
  self.reactor.handlers["selected"]=selectedHandler(ui,self)
--  self.reactor:register("selected",selectedHandler(ui,self))
end

return Menu



