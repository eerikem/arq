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
Reactor = require 'reactor'
UI = require 'ui_lib'
Graphic = require 'graphic'
Panel, List = require 'ui_obj'
Menu = require 'ui_menu'


local TITLE = Graphic:new("Title")
TITLE.align = "center"
TITLE:setBackgroundColor(colors.lightGray)
TITLE:setTextColor(colors.gray)

function setup_each()
  local w,h = term.getSize()
  local win = window.create(term.current(),w/2+2,1,w/2,h)
  win.setBackgroundColor(colors.gray)
  ui = UI:new(win)
end

--function test_new_Graphic()
--  local g = Graphic:new("Item")
--  luaunit.assertEquals(g.text,"Item")
--end
--
--function test_new_Panel()
--  local g = Graphic:new("Item")
--  local p = Panel:new()
--  luaunit.assertTrue(p.content)
--  p:add(g)
--  ui:add(p)
--  ui:update()
--  g.text="Item2"
--  p:redraw(ui)
--end
--
--function test_ui_align()
--  local g1 = Graphic:new("Item1")
--  local g2 = Graphic:new("Item2")
--  ui:update()
--  local ui2 = ui:newWindow(1,1,8,4)
--  ui2:add(g1,g2)
--  ui2:align("center","bottom")
--  ui:update()
--  ui2:update()
--end
--
--function test_list()
--  local l = List.fromArray({"Item1","Item2","Item3"})
--  l.align = "center" --todo Panel alignment
--  l:setTextColor(colors.orange)
--  ui:add(TITLE)
--  ui:add(l)
--  ui:update()
----  sleep(1)
--  l:add(TITLE)
--  ui:update()
--  sleep(1)
--end
--
--function test_mouse_listener()
--  local pressed = false
--  local handler = function(e) pressed = true end
--  local handler2 = function(e) pressed = false end
--  local g = Graphic:new("A really long button is here that should wrap around button")
--  luaunit.assertEquals(g.height,1)
--  ui:add(g)
--  local h = Graphic:new("Button2")
--  ui:add(h)
--  luaunit.assertEquals(g.height,3)
--  g:setOnSelect(ui,handler)
--  h:setOnSelect(ui,handler2)
--  local x = ui.term.getPosition()
--  ui.reactor:handleEvent("mouse_touch",x,5)
--  luaunit.assertFalse(pressed)
--  ui.reactor:handleEvent("mouse_touch",x,1)
--  luaunit.assertTrue(pressed)
--  ui.reactor:handleEvent("mouse_touch",x,4)
--  luaunit.assertFalse(pressed)
--end
--
--function test_button_list()
--  local button1 = false
--  local button2 = false
--  local handler1 = function(e) button1 = not button1 end
--  local handler2 = function(e) button2 = not button2 end
--  local a = Graphic:new("Button 1")
--  local b = Graphic:new("Button 2")
--  local l = List:new()
--  l.ypos = 2
--  l:add(a)
--  l:add(b)
--  b.ypos = 3
--  l:add(a)
--  a:setOnSelect(ui,handler1)
--  b:setOnSelect(ui,handler2)
--  ui:add(l)
--  ui:update()
--  local x = ui.term.getPosition()
--  ui.reactor:handleEvent("mouse_touch",x,2)
----  sleep(4)
--  luaunit.assertTrue(button1)
--  ui.reactor:handleEvent("mouse_touch",x,5)
----  sleep(4)
--  luaunit.assertTrue(button2)
--  ui.reactor:handleEvent("mouse_touch",x,6)
----  sleep(4)
--  luaunit.assertFalse(button1)
--end
--
function test_menu()
  local m = Menu.fromArray({"Item1","Item3","Item2"})
  luaunit.assertEquals(#m.index,3)
  ui:add(TITLE)
  m.xpos = 3
  m.ypos = 2
  ui:add(m)
  ui:update()
  sleep(1)
  local item4 = Graphic:new("Item4")  
  local m2 = Menu.fromArray({"Sub1","Sub2"})
  Menu.inlineOnFocus(item4,m2)
  m:add(item4)
  m.selected = 4
  ui:update()
  sleep(1)
  m.selected = 3
  ui:update()
  m:link(ui)
  local x,y = ui.term.getPosition()
  ui.reactor:handleEvent("scroll","scroll_down",x,y)
  sleep(1)
  ui.reactor:handleEvent("scroll","scroll_down",x,y)
  sleep(1)
  ui.reactor:handleEvent("scroll","scroll_up",x,y)
  sleep(1)
  ui.reactor:handleEvent("")
end
--
--local function assertColors(back,text)
--  return function(self,ui,noscroll,focus)
--    if focus then
--      self:colorFocus(ui.term)
--    else
--      self:color(ui.term)
--    end
--    local b = ui.term.getBackgroundColor()
--    local t = ui.term.getTextColor()
--    luaunit.assertEquals(b,back)
--    luaunit.assertEquals(t,text)
--    return self.proto.redraw(self,ui,noscroll,focus)
--  end
--end
--
--function test_color_inheritence()
--  local g = Graphic:new("The Text")
--  g.redraw = assertColors(colors.gray,colors.white)
--  ui:add(g)
--  ui:update()
--  ui:remove(g)
--  local p = Panel:new()
--  p:add(g)
--  p:setBackgroundColor(colors.white)
--  p:setTextColor(colors.black)
--  g.redraw = assertColors(colors.white,colors.black)
--  ui:add(p)
--  ui:update()
--  ui:remove(p)
--  p:remove(g)
--  --Now try double inheritence
--  local q = Panel:new()
--  q:add(g)
--  p:add(q)
--  ui:add(p)
--  ui:update()
--  g.redraw = assertColors(colors.white,colors.red)
--  q:setTextColor(colors.red)
--  ui:update()
--end

function tearDown_each()
  ui = nil
  console.restoreCursor()
end

print("EXIT: "..luaunit.LuaUnit.run())