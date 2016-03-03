
local Client = {}

function Client:new(Co)
  local ui = ui_sup.newWindow(Co,11,8)
  local l = List.fromArray(ui_sup.getUInames())
  ui:setBackground(colors.lightGray)
  ui:setText(colors.gray)
  local t = Graphic:new("UI List")
  t.align="center"
  t.ypos = 2
  local body = Panel:new()
  body.width = "max"
  l.xpos = 2
  l.ypos = 1
  local m = Menu.fromList(l)
  m.proto.backgroundFocus = colors.gray
  m.proto.textFocus = colors.white
--  m:setTextColor(colors.lightGray)
--  m:setBackgroundColor(colors.gray)
--  ui.term.setCursorBlink(true)
--  print("adding menu")sleep(2)
  body:add(m)
  body:setBackgroundColor(colors.gray)
  body:setTextColor(colors.lightGray)
  ui:add(t)
--  print("adding body")sleep(2)
  ui:add(body)
--  print("added body")sleep(1)
--  print("menu height: "..m.height)sleep(2)
--  print("height: "..body.height)sleep(2)
  m:link(ui)
  ui:align("center")
  ui:update()
  local x,y = ui.term.getPosition()
  VM.log(x.." "..y)
--  print("height: "..body.height)sleep(2)
end

return Client