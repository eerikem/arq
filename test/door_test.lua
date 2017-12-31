--Enable DEBUG to pause at the end of each test.
local DEBUG = true
local Door = require "door"
VM = require 'vm'

local Li
--local console = window.create(term.current(),1,1,w/3,h)
VM.log = function(str)
--  local old = term.redirect(console)
  print(str)
--  term.redirect(old)
end

local luaunit = require 'luaunit'
local gen_server = require 'gen_server'
local ui_sup = require 'ui_supervisor'
EVE = require "eventListener"
local ui_server = require 'ui_server'

---------------------
--Utility Functions--
---------------------

local function stop()
  while true do
    local event = {os.pullEvent()}
    if event[1] and event[2] and event[1] =="key_up"and event[2]== keys.space then
      break
    end
  end
end

function setup_each()
  VM.init()
  _, Li = EVE.start_link()
  local Ui = ui_sup.start_link(Li)
  local w,h = term.getSize()
  ui = ui_server.newWindow("terminal",w/2,h/2)
  ui:setBackground(colors.gray)
  ui:align("top","right")
end

function tearDown_each()
  if DEBUG then stop() end
  ui = nil
--  console.restoreCursor()
end

function runFor( nTime )
  local timer = os.startTimer( nTime or 0 )
  while true do
    local event = {os.pullEvent()}
    if event[1] and event[2] and event[1] == "timer" and event[2] == timer then
      break
    else
      gen_server.cast(Li,event)
    end
  end
end

function exec(cmd,...)
  if commands then
    commands.execAsync(string.format(cmd,unpack(arg)))
  else
    VM.log("Warning: Not a command computer")
  end
end

--------------
--Test Suite--
--------------

local doorCableColor = colors.white
local detectorCableColor = colors.black
local monitor = "terminal"
local ui_title = "Test"
local password = "123"

function test_detector()
  local door = Door.startDetectorDoor(doorCableColor,detectorCableColor,monitor,ui_title,1)
  local state = Door.getState(door)
  luaunit.assertEquals(Door.open(door),"opened")
  luaunit.assertEquals({Door.getState(door)},{true,false})
  runFor(1.5)
  luaunit.assertEquals({Door.getState(door)},{false,false})
end

function test_detector_pass()
  local door = Door.startDetectorDoor(doorCableColor,detectorCableColor,monitor,ui_title,1,123)
  local state = Door.getState(door)
  local co = coroutine.create(function()
    luaunit.assertEquals(Door.open(door),"opened")
    end)
  coroutine.resume(co)
  luaunit.assertEquals({Door.getState(door)},{false,false})
  os.queueEvent("char","1")
  os.queueEvent("char","2")
  os.queueEvent("char","3")
  runFor(1.5)
  luaunit.assertEquals({Door.getState(door)},{true,false})
end

print("EXIT: "..luaunit.LuaUnit.run())