local gen_server = require "gen_server"
local UI = require "lib.ui"
local Graphic = require "lib.graphic"
local Panel = require "lib.ui_obj"

local Client = {}

function Client.start_link(mon,title,audioCmd,time)
  
  local function init(ui)
    local title = Graphic:new(title)
    local body = Panel:new()
    local open = Graphic:new("PLAY")
    local cycler = Graphic:new("       ")
    title:align("center")
    body.width = "max"
    open.xpos = 2
    open.ypos = 2
    ui:add(title)
    body:add(open)
    body:add(cycler)
    ui:add(body)
    
    
    local function tock(A,B,C)
      cycler.text = A
      ui:update()
      EVE.tick(0.2)
      local r = VM.receive()
      if r == "wake" then
        return tock(B,C,A)
      elseif r == "stop" then
        cycler.text = "       "
        ui:update()
      else
        error("cycleHandler received bad msg")
      end
    end
    
    local function tick(dir)
      if dir == "left" then
        tock("  <  < "," <  <  ","<  <  <")
      elseif dir == "right" then
        tock(" >  >  ","  >  > ",">  >  >")
      else
        tock("  < >  "," <   > ","<     >")
      end
    end
    
    local ticker = nil
    
    local function cycle()
      --Avoid double cycling by checking for ticker
      if not ticker then
        ticker = VM.spawn(function()tick("right")end)
      end
    end
      
    local function stopTicker()
      if ticker then
  --      VM.log("Sending stop to ticker")
        VM.send(ticker,"stop")
        ticker = nil
      else
        error("No ticker to stop!",2)
      end
    end
    
    local function playAudio()
      if not ticker then
        exec(audioCmd)
        cycle()
        EVE.sleep(time)
        stopTicker()
      end
    end
    
    open:setOnSelect(ui,playAudio)
    
    
    local function bright()
      ui:setBackground(colors.lightGray)
      ui:setText(colors.blue)
      body:setTextColor(colors.lightBlue)
      body:setBackgroundColor(colors.blue)
      cycler:setTextColor(colors.lightBlue)
    end
      
    bright()
    ui:update()
    
    return ui
  end
  
  
  
  return UI.start(mon,7,5,init)
end

return Client