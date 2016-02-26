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
  end
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
  self:applyColors(ui)
  local first = true
  local counter = 0
  for n,V in ipairs(self.index) do
    if first then first = false
    else incCursorPos(ui.term,x) counter = counter + 1 end
    if n == self.selected then
      counter = counter + V:drawFocus(ui,noscroll)
    else
      counter = counter + V:redraw(ui,noscroll)
    end
  end
  return counter
end

return Menu




