--Enable DEBUG to pause at the end of each test.
--local DEBUG = true
local DEBUG = false

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
local EVE = require "eventListener"
local ui_server = require 'ui_server'
local Password = require 'password'

---------------------
--Utility Functions--
---------------------

local function stop()
  while true and DEBUG do
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
  ui:update()
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

function test_init()
  local pass = Password.start(123,"terminal")
  luaunit.assertTrue(pass)
end

function test_submitPassword()
  local key = "123"
  local pass = Password.start(key,"terminal")
  Password.setBuffer(pass,"321")
  luaunit.assertFalse(Password.submit(pass))
  Password.setBuffer(pass,key)
  luaunit.assertTrue(Password.submit(pass))
end

function test_display()
  local key = 123
  local pass = Password.start(key,"terminal")
  luaunit.assertEquals(Password.getDisplay(pass),'')
  gen_server.cast("terminal",{"char","1"})
  stop()
  luaunit.assertEquals(Password.getDisplay(pass),'*')
  luaunit.assertEquals(Password.getText(pass),"1")
  Password.clear(pass)
  luaunit.assertEquals(Password.getDisplay(pass),'')
  luaunit.assertEquals(Password.getText(pass),"")
end

function test_clear_button()
  local pass = Password.start(123,"terminal")
  Password.setBuffer(pass,"3214")
  stop()
  gen_server.cast("terminal",{"monitor_touch",7,5})
  luaunit.assertEquals(Password.getDisplay(pass),'')
end

function test_quit()

end


print("EXIT: "..luaunit.LuaUnit.run())