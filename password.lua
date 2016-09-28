local ui_server = require 'ui_server'
local gen_server = require 'gen_server'
local Panel = require "ui_obj"
local Graphic = require "graphic"
local Server = {}

function Server.submit(Co, key)
  return gen_server.call(Co,{"access",key})
end

function Server.start(key)
--  local ok, Co = Server.start_link("monitor_5",key)
  local ok, Co = Server.start_link("terminal",key)
  return Co  
end

function Server.start_link(Co,key)
  return gen_server.start_link(Server,{Co,key})
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
    delete = Graphic:new("-"),
    back = Graphic:new("q")
    }
    
  title.width = "max"
  title:setTextColor(colors.orange)
  local code = Graphic:new("    ***")
  
  local body = Panel:new()
  body:setLayout("static")
  
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
  body.width = "max"
  
  buttons.back.ypos = 3
  buttons.delete.ypos = 3
  buttons.delete.xpos = 7
  
  local bottom = Panel:new()
  bottom:setLayout("static")
  bottom:add(buttons.back)
  bottom:add(buttons.delete)
  bottom:setBackgroundColor(colors.lightGray)
  bottom:setTextColor(colors.gray)
  bottom.ypos = 3
  buttons.delete.ypos = 1
  buttons.back.ypos = 1
  body:add(bottom)
--  
--  body:add(buttons.back)
--  body:add(buttons.delete)
  
  ui:add(title)
  ui:add(code)
  ui:add(body)
  
  local isDark = false
  local function dark()
    code:setBackgroundColor(colors.black)
    code:setTextColor(colors.gray)
    buttons.back:setTextColor(colors.red)
    title:setBackgroundColor(colors.black)
    ui:setBackground(colors.gray)
    body:setTextColor(colors.lightGray)
    bottom:setBackgroundColor(nil)
    bottom:setTextColor(nil)
  end
  
  local function bright()
    title:setBackgroundColor(nil)
    code:setBackgroundColor(colors.gray)
    code:setTextColor(colors.lightGray)
    buttons.back:setTextColor(colors.red)
--    title:setBackgroundColor(colors.black)
    ui:setBackground(colors.lightGray)
    body:setBackgroundColor(colors.gray)
    body:setTextColor(colors.lightGray)
    bottom:setBackgroundColor(colors.lightGray)
    bottom:setTextColor(colors.gray)
  end
  
--  dark()
  bright()
  
  ui:update()
  
  
  local function colorHandler()
    if isDark then
      bright()
    else
      dark()
    end
    isDark = not isDark
    ui:update()
  end
  
  title:setOnSelect(ui,colorHandler)
  
  return ui
end

function Server.handle_call(Request,From,State)
  local event = Request[1]
  if event == "access" then
    local key = Request[2]
    if key == State.key then
      gen_server.reply(From,true)
      --TODO terminate process!
    else
      gen_server.reply(From,false)
    end
  else
    error("unrecognised event: ".. event)
  end
  return State
end

function Server.init(Co,key)
  local State = {ui = initUI(Co),key = key}
  return true, State
end

return Server