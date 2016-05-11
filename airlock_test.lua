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

gen_server = require 'gen_server'
EVE = require "eventListener"
luaunit = require 'luaunit'
ui_server = require 'ui_server'
Reactor = require 'reactor'
UI = require 'ui_lib'
Graphic = require 'graphic'
Panel, List = require 'ui_obj'
Menu = require 'ui_menu'
Radio = require 'ui_radio_panel'
Airlock = require 'airlock'

function setup_each()
  local w,h = term.getSize()
  local win = window.create(term.current(),w/2+2,1,w/2,h/2)
  win.setBackgroundColor(colors.gray)
  VM.init()
  local Li = EVE.start_link()
  local Co = ui_server.start_link(term.current(),"Terminal")
  VM.register("terminal",Co)
  ui = UI:new(win)
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
  luaunit.assertEquals(res,"opened")
  --Open already open door
  res = Airlock.open(lock,"inner")
  luaunit.assertEquals(res,"open")
  luaunit.assertTrue(State.doors.inner:isOut())
  --Open closed door when other is open
  res = Airlock.open(lock,"outer")
  luaunit.assertEquals(res,"cycling")
  luaunit.assertFalse(State.doors.inner:isOut())
  luaunit.assertTrue(State.doors.outer:isOut())
  --Close already open door
  res = Airlock.close(lock,"outer")
  luaunit.assertEquals(res,"closed")
  luaunit.assertFalse(State.doors.outer:isOut())
  --Close already closed door
  res = Airlock.close(lock,"outer")
  luaunit.assertEquals(res,"not_open")
end

function tearDown_each()
  ui = nil
  console.restoreCursor()
end

print("EXIT: "..luaunit.LuaUnit.run())