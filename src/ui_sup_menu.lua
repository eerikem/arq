local Observer = require "observer"
local Graphic = require "lib.graphic"
local Menu = require "lib.ui_menu"
local Panel, List = require "lib.ui_obj"
local ui_sup = require "ui_supervisor"
local ui_server = require "ui_server"

local Client = {}

function Client:new(Co)
  local ui = ui_sup.newWindow(Co,12,15)
  local l = List.fromArray(ui_sup.getUInames())
  ui:setBackground(colors.lightGray)
  ui:setText(colors.gray)
  local t = Graphic:new("UI List")
  t:align("center")
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
  
  local function logSizeHandler(monitor)
    return function()
      VM.log(string.format("%s is %d by %d",monitor,ui_server.getSize(monitor)))
    end
  end
  
  for _,g in ipairs(m.index) do
    g:setJustOnSelect(ui,logSizeHandler(g.text))
  end
  
end

return Client