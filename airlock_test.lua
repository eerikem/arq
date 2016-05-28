--local env = {}
--setmetatable(env,{__index = _ENV})
--setfenv(1,env)
--_ENV = getfenv()
--assert(_ENV)
VM = require 'vm'

local w,h = term.getSize()
local console = window.create(term.current(),1,1,w/3,h)
console.clear()
console.setCursorPos(1,1)
console.setCursorBlink(true)
function writeConsole(str)
  local old = term.redirect(console)
  print(str)
  term.redirect(old)
end

--colors = {white = "white",yellow = "yellow"}

--local sleep = function() end

VM.log = writeConsole

Reactor = require 'reactor'
gen_server = require 'gen_server'
EVE = require "eventListener"
luaunit = require 'luaunit'
UI = require 'ui_lib'
ui_sup = require 'ui_supervisor'
Graphic = require 'graphic'
Panel, List = require 'ui_obj'
Menu = require 'ui_menu'
Radio = require 'ui_radio_panel'
Airlock = require 'airlock'

function setup_each()
  VM.init()
  local Li = EVE.start_link()
  local Ui = ui_sup.start_link(Li)
  
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
  
end

function test_init_lock()
  local lock = Airlock.start_link()
  luaunit.assertEquals(type(lock),"thread")
  local State = gen_server.call(lock,{"observer"})
  luaunit.assertFalse(State.doors.inner:isOut())
  luaunit.assertFalse(State.doors.outer:isOut())
  Airlock.open(lock,"inner")
end

function test_ui()
  local ui,ui2 = Airlock.testUI("terminal")
  ui2:align("right")
  ui:update()
end

function test_open_close()
  local lock = Airlock.start_link()
  local State = gen_server.call(lock,{"observer"})
  --Open closed door
  local res = Airlock.open(lock,"inner")
  luaunit.assertEquals(res,"opening")
  luaunit.assertTrue(State.doors.inner:isOut())
  runFor(2)
  res = Airlock.open(lock,"inner")
  luaunit.assertEquals(res,"opening")
  runFor(2)
  --Open already open door
  res = Airlock.open(lock,"inner")
  luaunit.assertEquals(res,"open")
  luaunit.assertTrue(State.doors.inner:isOut())
  --Close already open door
  res = Airlock.close(lock,"inner")
  luaunit.assertEquals(res,"closing")
  luaunit.assertFalse(State.doors.outer:isOut())
  runFor(2)
  res = Airlock.close(lock,"inner")
  luaunit.assertEquals(res,"closing")
  runFor(2)
  --Close already closed door
  res = Airlock.close(lock,"inner")
  luaunit.assertEquals(res,"closed")
  --Close opening door
  res = Airlock.open(lock,"inner")
  runFor(1)
  luaunit.assertEquals(res,"opening")
  luaunit.assertTrue(State.doors.inner.opening)
  res = Airlock.close(lock,"inner")
  luaunit.assertEquals(res,"closing")
  luaunit.assertFalse(State.doors.inner.opening)
  luaunit.assertTrue(State.doors.inner.closing)
  runFor(1.5)
  luaunit.assertFalse(State.doors.inner.closing)
  res = Airlock.close(lock,"inner")
  luaunit.assertEquals(res,"closed")
  --Open closing door
  Airlock.open(lock,"inner")
  runFor(4)
  luaunit.assertEquals(Airlock.open(lock,"inner"),"open")
  Airlock.close(lock,"inner")
  runFor(1)
  luaunit.assertTrue(State.doors.inner.closing)
  res = Airlock.open(lock,"inner")
  luaunit.assertEquals(res,"opening")
  luaunit.assertTrue(State.doors.inner.opening)
  luaunit.assertFalse(State.doors.inner.closing)
  runFor(1.5)
  luaunit.assertEquals(Airlock.open(lock,"inner"),"open")
  
--  --Open closed door when other is open
--  res = Airlock.open(lock,"outer")
--  luaunit.assertEquals(res,"cycling")
--  runFor(5)
--  luaunit.assertFalse(State.doors.inner:isOut())
--  luaunit.assertTrue(State.doors.outer:isOut())
end

function tearDown_each()
  ui = nil
  console.restoreCursor()
end

print("EXIT: "..luaunit.LuaUnit.run())