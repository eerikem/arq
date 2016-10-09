local ui_server = require 'ui_server'
local gen_server = require 'gen_server'
local Panel = require "ui_obj"
local Graphic = require "graphic"
local Server = {}

function Server.submit(Co)
  return gen_server.call(Co,{"access"})
end

function Server.setBuffer(Co, key)
  gen_server.cast(Co,{"set_buffer",key})
end

function Server.getText(Co)
  return gen_server.call(Co,{"buffer"})
end

--Expects an access key, parent ui and optional Module function argument tuple for callback.
function Server.start(key, ui, ModFunArg)
  if key == nil then
    error("Password requires a key",2) end
  if type(key) == "number" then
    key = "" .. key end
  local ok, Co = Server.start_link(key,ui,ModFunArg)
  VM.register("password",Co)
  return Co  
end

function Server.back(Co)
  gen_server.cast(Co,{"back"})
end

function Server.sendChar(Co,char)
  gen_server.cast(Co,{"char",char})
end

function Server.clear(Co)
  gen_server.cast(Co,{"clear"})
end

function Server.quit(Co)
  gen_server.cast(Co,{"quit"})
end

function Server.getDisplay(Co)
  return gen_server.call(Co,{"display"})
end

function Server.start_link(Co,key)
--  return gen_server.start_link(Server,{Co,key})
  return gen_server.start(Server,{Co,key})
end

local function initUI(Co)
  local ui = ui_server.newWindow(Co,7,5 )
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
    clear = Graphic:new("c"),
    back = Graphic:new("q")
    }
    
  title.width = "max"
  title:setTextColor(colors.orange)
  local code = Graphic:new("       ")
  
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
  buttons.clear.ypos = 3
  buttons.clear.xpos = 7
  
  local bottom = Panel:new()
  bottom:setLayout("static")
  bottom:add(buttons.back)
  bottom:add(buttons.clear)
  bottom:setBackgroundColor(colors.lightGray)
  bottom:setTextColor(colors.gray)
  bottom.ypos = 3
  buttons.clear.ypos = 1
  buttons.back.ypos = 1
  body:add(bottom)
--  
--  body:add(buttons.back)
--  body:add(buttons.clear)
  
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
  
  --Prepare handlers!--
  local Co = VM.running()
  local function keyHandler(event, code, isHeld)
    assert(event == "key")
    if  ( code >= 2 and code <= 11 ) or
        ( code >= 16 and code <=25 ) or
        ( code >= 30 and code <=38 ) or
        ( code >= 44 and code <=50 ) or
        ( code >= 71 and code <=73 ) or
        ( code >= 75 and code <=77 ) or
        ( code >= 79 and code <=82 ) then
        Server.sendChar(Co,keys.getName( code ))
    elseif code == 28 then
      Server.submit(Co)
    elseif code == 14 or code == 211 then
      
    else
      ui:beep()
    end
  end
  
  for name, button in pairs(buttons) do
    if name ~= "clear" and name ~= "back" then
      button:setOnSelect(ui,function() Server.sendChar(Co,button.text) end)
    end
  end
  
  local function charHandler(event,char)
    assert(event == "char")
    Server.sendChar(Co,char)
  end
  ui:register(charHandler,"char")
  
  buttons.clear:setOnSelect(ui,function() Server.clear(Co) end)
  buttons.back:setOnSelect(ui,function() Server.back(Co) end)
  return ui,code
end

local function trim(s)
  return s:match'^%s*(.*%S)' or ''
end

local function star(s)
  local r = ""
  for n=1, string.len(s) do
    r = r.. "*"
  end
  return r
end

local function fill(s)
  local r = ""
  s = trim(s)
  for n=1, 7 - string.len(s) do
    r = r.. " "
  end
  return r .. s
end

function Server.handle_cast(Request,State)
  local event = Request[1]
  if event == "set_buffer" then
    local buf = Request[2]
    State.buffer=buf
    State.display.text=fill(star(buf))
    State.ui:update()
  elseif event == "char" then
    local char = Request[2]
    State.buffer=State.buffer..char
    if string.len(State.buffer) <= 7 then
      State.display.text=fill(State.display.text.."*")
    end
    if State.buffer == State.key then
      --TODO password match success!
      State.ui:ping()
      State.display:setTextColor(colors.green)
      State.ui:update()
      EVE.sleep(1)
      VM.exit("normal")
    else
      State.ui:tap()
      State.ui:update()
    end
  elseif event == "clear" then
    State.buffer=""
    State.display.text="       "
    State.ui:ping()
    State.ui:update()
  elseif event == "back" then
    --TODO terminate returning focus to parent???
    if State.callback then
      local Module, fun, args = unpack(State.callback)
      Module[fun](unpack(args))
      State.ui:beep()
    else
      State.ui:beep()
      VM.exit("normal")
    end
  else
    error("unrecognised event: ".. event)
  end
  return State
end

function Server.handle_call(Request,From,State)
  local event = Request[1]
  if event == "access" then
    if State.buffer == State.key then
      gen_server.reply(From,true)
      --TODO terminate process!
    else
      gen_server.reply(From,false)
    end
  elseif event == "display" then
    gen_server.reply(From,trim(State.display.text))
  elseif event == "buffer" then
    gen_server.reply(From,trim(State.buffer))
  else
    error("unrecognised event: ".. event)
  end
  return State
end

function Server.init(key,Co,MFA)
  local ui,display = initUI(Co)
  local State = {ui = ui,display = display,key = key,buffer="",callback=MFA}
  return true, State
end

return Server