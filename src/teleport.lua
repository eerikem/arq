local gen_server = require "gen_server"
local ui_server = require "ui_server"
local Graphic = require "graphic"
local Panel = require "ui_obj"

local Tele = {}

local endtp = "/tpx @p 1 56 48 27"
local home = "/tpx @p 0 97 79 32"

local function teleUI(Co)
  local ui = ui_server.newWindow(Co,7,5)
  
  local button = Graphic:new("TPX END")
  
  local buttonPanel = Panel:new()
  buttonPanel:setLayout("static")
  button.ypos = 2
  buttonPanel.ypos = 2
  buttonPanel:add(button)
  
  buttonPanel:setOnSelect(ui,function()exec(endtp)end)
  
  buttonPanel.width = "max"
  
  ui:add(buttonPanel)
  
  ui:setBackground(colors.lightGray)
  ui:setText(colors.gray)
  buttonPanel:setBackgroundColor(colors.gray)
  buttonPanel:setTextColor(colors.orange)
  
  ui:align("bottom")
  ui:update()
    
  return ui
end


function Tele.start()
  return gen_server.start_link(Tele,{},{})
end

function Tele.init()
  local ui = teleUI("monitor_1")
  return true, {ui =ui}
end

function Tele.handle_call(Request,From,State)
  gen_server.reply(From,"ok")
  return State
end

function Tele.handle_cast(Request,State)
  local event = Request[1]
  return State
end

return Tele