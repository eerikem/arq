local Panel, List = require 'ui_obj'

local radioIndex = 0

local Radio = Panel:new()
Radio.id = "radio"
Radio.focus = 1

function Radio.fromList(list)
  radioIndex = radioIndex + 1
  local m = Radio:new()
  m.id = "menu"..radioIndex
  for _,V in ipairs(list.index) do
    m:add(V)
    --todo, hard overwrite of proto side effects?!?
    V.proto = m.proto
  end
  m.ypos = list.ypos
  m.xpos = list.xpos
  return m
end

function Radio.fromArray(array)
  return Radio.fromList(List.fromArray(array))
end

function Radio:drawItem(ui,obj,noscroll)
  if obj == self.index[self.focus] then
    return obj:drawFocus(ui,noscroll)
  else
    return obj:redraw(ui,noscroll)
  end
end

function Radio:setSelected(index)
  if not index then
    error("setSelected requires a numeric index",2) end
  if index < 1 or index > #self.index then
    error("bad index "..index,2) end
  self.focus = index
end

function Radio:noneSelected()
  self.focus = -1
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


function Radio:link(ui)
  ui:register(self,"clickable")
  self.reactor:register("mouse_click",focusHandler(ui,self))
  self.reactor:register("monitor_touch",focusHandler(ui,self))
  self.reactor:register("mouse_up",mouseUpHandler(ui,self))
end

return Radio



