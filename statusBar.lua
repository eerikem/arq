local ui_server = require 'ui_server'
local gen_server = require 'gen_server'
local Reactor = require 'reactor'

local Bar = {buffer = 512,line = 1}

local function scrollHandler(Bar)
  local ui = Bar.ui
  return function(_,direction)
    if direction == "scroll_up" then
--      writeConsole(Bar.line)
      if Bar.line > 1 then
        Bar.line = Bar.line - 1
        ui.term.setCursorPos(1,1)
        ui.pane:drawFromLine(ui,Bar.line)
        ui:redraw()
      else
        ui:beep()
      end
    elseif direction == "scroll_down" then
      local _,height = ui.term.getSize()
      if Bar.line < ui.pane.height - height + 1 then 
        Bar.line = Bar.line + 1
        ui.term.setCursorPos(1,1)
        ui.pane:drawFromLine(ui,Bar.line)
        ui:redraw()
      else
        ui:beep()
      end
    end
  end
end

local function msgHandler(Bar)
  local MSG_CNT = 0
  return function (_,str)
    MSG_CNT = MSG_CNT + 1
    local n = 0
    if string.find(string.lower(str),'error') then
      n = Bar.ui:add(Graphic:new({text = MSG_CNT.." "..str,textColor=colors.red}))
    else
      n = Bar.ui:add(Graphic:new(MSG_CNT.." "..str))
    end
    local _,height = Bar.ui.term.getSize()
    
    if Bar.ui.pane.height > height then
      Bar.line = Bar.line + n
      Bar.ui.term.setCursorPos(1,1)
      Bar.ui.pane:drawFromLine(Bar.ui,Bar.line)
    end
    Bar.ui:redraw()
  end
end

function Bar:new(Co,height)
  if not VM.registered(Co) then error("Arg1 must be a ui_server") end
  local height = height or 6
  
  local ui = ui_server.newWindow(Co,"max",height)
  ui:align("bottom","left")
  ui:setBackground(colors.black)
  ui:setText(colors.gray)
  ui:update()
   
  local o = {parent = Co,height = height,ui = ui,reactor = Reactor:new()}
  setmetatable(o,self)
  self.__index = self
  
  o.reactor:register("scroll",scrollHandler(o))
  o.reactor:register("message",msgHandler(o))
  ui:register(o,"scroll")
  --TODO integrate into supervision tree?
  o.Co = gen_server.start_link(Bar,{o},{})
  return o
end

function Bar.init(bar)
  return bar
end

function Bar.handle_call(Request,From,State)
  return State
end

function Bar.handle_cast(Request,bar)
  local event = unpack(Request)
    if event == "message" or "scroll" then
      bar.reactor:handleEvent(unpack(Request))
    end
  return bar
end

function Bar:write(msg)
--  self.ui:add(Graphic:new(msg))
  gen_server.cast(self.Co,{"message",msg})
end

return Bar