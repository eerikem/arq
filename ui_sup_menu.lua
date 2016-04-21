
local Client = {}

function Client:new(Co)
  local ui = ui_sup.newWindow(Co,11,10)
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
  body:add(m)
  body:setBackgroundColor(colors.gray)
  body:setTextColor(colors.lightGray)
  ui:add(t)
  ui:add(body)
  m:link(ui)
  ui:align("right")
  ui:update()
  
  local function fun()
    t.text = "Flush "..FLUSHER
    ui:update()
  end
  t:setOnSelect(ui,fun)
  
  ui:update()
end

return Client