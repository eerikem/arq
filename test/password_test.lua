--Enable DEBUG to pause at the end of each test.
local DEBUG = true

VM = require 'vm'
local Li
local console
VM.log = function(str)
--  local old = term.redirect(console)
  print(str)
--  term.redirect(old)
end

local luaunit = require 'luaunit'
local gen_server = require 'gen_server'
local ui_sup = require 'ui_supervisor'
local EVE = require "eventListener"
local ui_server = require 'ui_server'
local Password = require 'password'

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
  Li = EVE.start_link()
  local Ui = ui_sup.start_link(Li)
  local w,h = term.getSize()
  ui = ui_server.newWindow("terminal",w/2,h/2)
  ui:setBackground(colors.gray)
  ui:align("top","right")
end

function tearDown_each()
  if DEBUG then stop() end
  ui = nil
  DEBUG = false
  console.restoreCursor()
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

--------------
--Test Suite--
--------------

function test_init()
  local pass = Password.start()
  luaunit.assertTrue(pass)
end


print("EXIT: "..luaunit.LuaUnit.run())