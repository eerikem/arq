local ui_server = require 'ui_server'
local gen_server = require 'gen_server'
local Panel = require "ui_obj"
local Graphic = require "graphic"
local Server = {}

function Server.start()
  return Server.start_link("monitor_1")
--  return Server.start_link("terminal")  
end

function Server.start_link(Co)
  return gen_server.start_link(Server,{Co})
end

local function initUI(Co)
  local ui = ui_server.newWindow(Co,7,5)
--  local ui = ui_server.newWindow(Co,12,10)
  
  local title = Graphic:new("ACCESS#")
  
  local buttons = {
    zero = Graphic:new("0"),
    one = Graphic:new("1"),
    two = Graphic:new("2"),
    three = Graphic:new("3"),
    four = Graphic:new("4"),
    five = Graphic:new("5"),
    six = Graphic:new("6"),
    delete = Graphic:new("del"),
    back = Graphic:new("<-")
    }
    
  title.width = "max"
  title:setTextColor(colors.orange)
  title:setBackgroundColor(colors.black)
  local code = Graphic:new("    ***")
  code:setBackgroundColor(colors.black)
  code:setTextColor(colors.gray)
  
  buttons.back:setTextColor(colors.red)
  
  local body = Panel:new()
  body:setLayout("static")
  body:setTextColor(colors.lightGray)
  
  buttons.one.xpos = 2
  buttons.four.xpos = 2
  
  buttons.two.xpos = 4
  buttons.five.xpos = 4
  
  buttons.three.xpos = 6
  buttons.six.xpos = 6
  
  buttons.four.ypos = 2
  buttons.five.ypos = 2
  buttons.six.ypos = 2
  
  body:add(buttons.one)
  body:add(buttons.two)
  body:add(buttons.three)
  body:add(buttons.four)
  body:add(buttons.five)
  body:add(buttons.six)
  
  buttons.back.ypos = 3
  buttons.delete.ypos = 3
  buttons.delete.xpos = 5
  body:add(buttons.back)
  body:add(buttons.delete)
  
  ui:add(title)
  ui:add(code)
  ui:add(body)
  
  ui:setBackground(colors.gray)
  ui:update()
  return ui
end

function Server.init(Co)
  local State = {ui = initUI(Co)}
  return true, State
end

return Server