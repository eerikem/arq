local menuIndex = 0

local Menu = Panel:new()
Menu.id = "menu"
Menu.selected = 1
Menu.listeners = {"key","mouse_click","monitor_touch"}
Menu.handlers = {keyHandler}--TODO fix this?!?
function Menu:fromList(list)
  local m = Menu:new(list)
  return m
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




