local env = {}
setmetatable(env,{__index = _ENV})
setfenv(1,env)
_ENV = getfenv()
assert(_ENV)
local VM = require 'vm'

local w,h = term.getSize()
local console = window.create(term.current(),1,1,w,h)
console.clear()
console.setCursorPos(1,1)
console.setCursorBlink(true)

luaunit = require 'luaunit'
UI = require 'ui_lib'
Graphic = require 'graphic'
Panel, List = require 'ui_obj'
Menu = require 'ui_menu'

function setup_each()
  local w,h = term.getSize()
  local win = window.create(term.current(),w/2+2,1,w/2,h)
  win.setBackgroundColor(colors.gray)
  ui = UI:new(win)
end

function test_new_Graphic()
  local g = Graphic:new("Item")
  luaunit.assertEquals(g.text,"Item")
end

function test_new_Panel()
  local g = Graphic:new("Item")
  local p = Panel:new()
  luaunit.assertTrue(p.content)
  p:add(g)
  ui:add(p)
  ui:update()
  g.text="Item2"
  p:redraw(ui)
end

function test_ui_align()
  local g1 = Graphic:new("Item1")
  local g2 = Graphic:new("Item2")
  ui:update()
  local ui2 = ui:newWindow(1,1,8,4)
  ui2:add(g1,g2)
  ui2:align("center","bottom")
  ui:update()
  ui2:update()
end

function test_list()
  local l = List.fromArray({"Item1","Item2","Item3"})
  local t = Graphic:new("Title")
  t.align = "center"
  t:setBackgroundColor(colors.lightGray)
  t:setTextColor(colors.gray)
  l.align = "center" --todo Panel alignment
  l:setTextColor(colors.orange)
  ui:add(t)
  ui:add(l)
  ui:update()
  sleep(1)
  l:add(t)
  ui:update()
  sleep(2)
end

function test_menu()
  local l = List.fromArray({"Item1","Item2","Item3"})
  local m = Menu.fromList(l)
  
end

function tearDown_each()
  ui = nil
  console.restoreCursor()
end

print("EXIT: "..luaunit.LuaUnit.run())