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

local sleep = function() end

VM.log = writeConsole

gen_server = require 'gen_server'
luaunit = require 'luaunit'
ui_server = require 'ui_server'
Reactor = require 'reactor'
UI = require 'ui_lib'
Graphic = require 'graphic'
Panel, List = require 'ui_obj'
Menu = require 'ui_menu'
Radio = require 'ui_radio_panel'

local TITLE = Graphic:new("Title")
TITLE.align = "center"
TITLE:setBackgroundColor(colors.lightGray)
TITLE:setTextColor(colors.gray)

function setup_each()
  local w,h = term.getSize()
  local win = window.create(term.current(),w/2+2,1,w/2,h/2)
  win.setBackgroundColor(colors.gray)
  VM.init()
  local Co = ui_server.start_link(term.current(),"Terminal")
  VM.register("terminal",Co)
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
  l.align = "center" --todo Panel alignment
  l:setTextColor(colors.orange)
  ui:add(TITLE)
  ui:add(l)
  ui:update()
--  sleep(1)
  l:add(TITLE)
  ui:update()
  sleep(1)
end

function test_mouse_listener()
  local pressed = false
  local handler = function(e) pressed = true end
  local handler2 = function(e) pressed = false end
  local g = Graphic:new("A really long button is here that should wrap around button")
  luaunit.assertEquals(g.height,1)
  ui:add(g)
  local h = Graphic:new("Button2")
  ui:add(h)
  luaunit.assertEquals(g.height,3)
  g:setOnSelect(ui,handler)
  h:setOnSelect(ui,handler2)
  local x = ui.term.getPosition()
  ui.reactor:handleEvent("mouse_click",1,1,x,5)
  ui.reactor:handleEvent("mouse_up",1,1,x,5)
  luaunit.assertFalse(pressed)
  ui.reactor:handleEvent("mouse_click",2,1,x,1)
  ui.reactor:handleEvent("mouse_up",2,1,x,1)
  luaunit.assertTrue(pressed)
  ui.reactor:handleEvent("mouse_click",3,1,x,4)
  ui.reactor:handleEvent("mouse_up",3,1,x,4)
  luaunit.assertFalse(pressed)
end

function test_button_list()
  local button1 = false
  local button2 = false
  local handler1 = function(e) button1 = not button1 end
  local handler2 = function(e) button2 = not button2 end
  local a = Graphic:new("Button 1")
  local b = Graphic:new("Button 2")
  local l = List:new()
  l.ypos = 2
  l:add(a)
  l:add(b)
  b.ypos = 3
--  l:add(a)
  a:setOnSelect(ui,handler1)
  b:setOnSelect(ui,handler2)
  luaunit.assertTrue(ui.selectables[a] and ui.selectables[b])
  ui:add(l)
  ui:update()
  local x = ui.term.getPosition()
  ui.reactor:handleEvent("mouse_click",1,1,x,2)
  ui.reactor:handleEvent("mouse_up",1,1,x,2)
  sleep(1)
  luaunit.assertTrue(button1)
  ui.reactor:handleEvent("mouse_click",2,1,x,5)
  ui.reactor:handleEvent("mouse_up",2,1,x,5)
  sleep(1)
  luaunit.assertTrue(button2)
  --TODO add supports duplicate objects..?
--  ui.reactor:handleEvent("mouse_click",3,1,x,6)
--  ui.reactor:handleEvent("mouse_up",3,1,x,6)
--  sleep(4)
--  luaunit.assertFalse(button1)
end

function test_getSize()
  local width = 20
  local ui = UI:new(window.create(term.current(),1,1,width,8))
  ui:setBackground(colors.lightGray)
  local t = Graphic:new("TEST HEIGHT")
  luaunit.assertEquals({t:getSize(width)},{11,1})
  t.xpos = 4
  t.ypos = 2
  luaunit.assertEquals({t:getSize(width)},{14,2})
  t:setTextColor(colors.red)
  ui:add(t)
  ui:add(Graphic:new("First line is here"))
  ui:add(Graphic:new("A single line text"))
  local g = Graphic:new("This text ends on a second line just fit")
  ui:add(g)
  luaunit.assertEquals({g:getSize(width)},{width,2})
  ui:update()
  luaunit.assertEquals({ui.pane:getSize(width)},{width,6})
  ui.pane.xpos = 2
  ui.pane.ypos = 2
  ui:update()
  luaunit.assertEquals({ui.pane:getSize(width)},{width,8})
end

function test_lineWrap()
  local width = 7
  local string = "Hello"
  luaunit.assertEquals({UI.lineWrap(string,width)},{string})
  string = "This is a simple line"
  luaunit.assertEquals({UI.lineWrap(string,width)},{"This is ","a simple line"})
end

function test_drawLine()
  local width = 20
  local ui = UI:new(window.create(term.current(),1,1,width,9))
  ui:setBackground(colors.lightGray)
  local t = Graphic:new("TEST DRAWLINE")
  t.align = "center"
  t:setTextColor(colors.red)
  ui:add(t)
  ui:add(Graphic:new("First line is here"))
  ui:add(Graphic:new("A single line text"))
  local g = Graphic:new("This text ends on a second line just fit")
  ui:add(g)
  luaunit.assertEquals(g:getTextFromLine(2,width),"second line just fit")
  ui.pane.xpos = 2
  ui:update()
  luaunit.assertEquals(g:getTextFromLine(3,width - 1),"fit")
  ui.term.setCursorBlink(true)
  incCursorPos(ui.term,1)
  sleep(1)
  ui.pane:drawFromLine(ui,3)
end


function test_status()
  local ui = UI:new(window.create(term.current(),1,1,term.getSize()))
  ui:add(Graphic:new("Text starts here"))
  ui:add(Graphic:new("Some more text here"))
  ui:add(Graphic:new("Text ends on this line"))
  ui:update() sleep(1)
  ui.pane:drawFromLine(ui,2)
  sleep(1)
  ui.pane:drawFromLine(ui,3)
  sleep(1)
  
  local statusBar = require 'statusBar'
  local status = statusBar:new("terminal",4)
  status:write("First")
  luaunit.assertStrContains(status.ui.pane.index[1].text,"First")
  status:write("Message")writeConsole("hello: "..table.concat({status.ui.pane:getSize(10)}," ").." ")
  luaunit.assertStrContains(status.ui.pane.index[2].text,"Message")
  status:write("Message")--write(status.ui.pane.height)
  status:write("Message")--write(status.ui.pane.height)
  status:write("Message")
  status:write("Message")
  status:write("Last")
  sleep(1)
  gen_server.cast("terminal",{"mouse_scroll",-1,2,17})
  sleep(1)
  gen_server.cast("terminal",{"mouse_scroll",-1,2,17})
  sleep(1)
  gen_server.cast("terminal",{"mouse_scroll",-1,2,17})
  sleep(1)
  gen_server.cast("terminal",{"mouse_scroll",-1,2,17})
  sleep(1)
  gen_server.cast("terminal",{"mouse_scroll",1,2,17})
  gen_server.cast("terminal",{"mouse_scroll",1,2,17})
  gen_server.cast("terminal",{"mouse_scroll",1,2,17})
  gen_server.cast("terminal",{"mouse_scroll",1,2,17})
end

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
  m.focus = 4
  ui:update()
  sleep(1)
  m.focus = 3
  ui:update()
  m:link(ui)
  local x,y = ui.term.getPosition()
  ui.reactor:handleEvent("scroll","scroll_down",x,y)
  sleep(1)
  ui.reactor:handleEvent("scroll","scroll_down",x,y)
  sleep(1)
  ui.reactor:handleEvent("scroll","scroll_up",x,y)
  sleep(1)
  print(m.index[3].absY)sleep(1)
  m.index[3].reactor:register("selected",function() VM.log(m.index[3].text.." selected") end)
  x = x + m.xpos - 1
  ui.reactor:handleEvent("mouse_up",101,1,x,5)
  ui.reactor:handleEvent("mouse_click",123,1,x,5)
  ui.reactor:handleEvent("mouse_up",123,1,x,5)
  sleep(1)
end

local function assertColors(back,text)
  return function(self,ui,noscroll,focus)
    if focus then
      self:colorFocus(ui.term)
    else
      self:color(ui.term)
    end
    local b = ui.term.getBackgroundColor()
    local t = ui.term.getTextColor()
    luaunit.assertEquals(b,back)
    luaunit.assertEquals(t,text)
    return self.proto.redraw(self,ui,noscroll,focus)
  end
end

function test_color_inheritence()
  local g = Graphic:new("The Text")
  g.redraw = assertColors(colors.gray,colors.white)
  ui:add(g)
  ui:update()
  ui:remove(g)
  local p = Panel:new()
  p:add(g)
  p:setBackgroundColor(colors.white)
  p:setTextColor(colors.black)
  g.redraw = assertColors(colors.white,colors.black)
  ui:add(p)
  ui:update()
  ui:remove(p)
  p:remove(g)
  --Now try double inheritence
  local q = Panel:new()
  q:add(g)
  p:add(q)
  ui:add(p)
  ui:update()
  g.redraw = assertColors(colors.white,colors.red)
  q:setTextColor(colors.red)
  ui:update()
end

function test_menu_interactions()
  local ui = ui_server.newWindow("terminal",7,5)
  ui:setBackground(colors.gray)
  ui:setText(colors.lightGray)
  ui:align("center")
  
  local item1 = Graphic:new("Item1")
  local item2 = Graphic:new("Item2")
  local item3 = Graphic:new("Item3")
  
  local lastSelected = "none"
  local function selectedHandler(item)
    return "selected", function(event)
      lastSelected = item.text
    end
  end
  
  item1.reactor:register(selectedHandler(item1))
  item2.reactor:register(selectedHandler(item2))
  item3.reactor:register(selectedHandler(item3))
  
  local menu = Menu:new()
  menu:add(item1)
  menu:add(item2)
  menu:add(item3)
  
  menu.xpos = 2
  menu.ypos = 2
  
  ui:add(menu)
  ui:update()
  
  menu:link(ui)
  
  local x,y = ui.term.getPosition()
  x = x + menu.absX
  y = y + menu.absY
  
  term.setTextColor(colors.white)
  term.setCursorPos(x,y)
  term.setCursorBlink(true)
  sleep(1)
  
  gen_server.cast("terminal",{"mouse_scroll",1,x,y})
  luaunit.assertEquals(menu.focus,2)
  gen_server.cast("terminal",{"mouse_click",1,1,x,y+2})
  sleep(1)
  luaunit.assertEquals(menu.focus,3)
  luaunit.assertEquals(lastSelected,"none")
  gen_server.cast("terminal",{"mouse_click",2,1,x,y})
  gen_server.cast("terminal",{"mouse_up",2,1,x,y})
  luaunit.assertEquals(menu.focus,1)
  luaunit.assertEquals(lastSelected,"Item1")
  
  gen_server.cast("terminal",{"key",keys.enter})
end

local clickCounter = 0
local function mouseClick(ui,x,y)
  local xPos,yPos = ui.term.getPosition()
  x = x + xPos - 1
  y = y + yPos - 1
  ui.reactor:handleEvent("mouse_click",clickCounter,1,x,y)
  ui.reactor:handleEvent("mouse_up",clickCounter,1,x,y)
  clickCounter = clickCounter + 1
end

function test_static_layout()
  local pane = Panel:new()
  pane:setLayout("static")
  local g = Graphic:new("button1")
  local h = Graphic:new("button2")
  local pressed
  g:setOnSelect(ui,function()pressed=g.text end)
  h:setOnSelect(ui,function()pressed=h.text end)
  
  h.xpos = 2
  h.ypos = 3
  g.xpos= 3
  
  pane:add(h)
  pane:add(g)
  ui:add(pane)
  ui:update()
  sleep(1)
  
  luaunit.assertEquals(pane.height,3)
  luaunit.assertEquals(pane.width,9)
  
  mouseClick(ui,3,1)
  luaunit.assertEquals(pressed,"button1")
  mouseClick(ui,2,3)
  luaunit.assertEquals(pressed,"button2")
end

function test_radio_panel()
  local pane = Radio:new()
  pane:setLayout("static")
  
  local g = Graphic:new("button1")
  local h = Graphic:new("button2")
  g.xpos = 3
  g.ypos = 2
  
  pane:add(g)
  pane:add(h)
  
  ui:add(pane)
  ui:update()
  sleep(1)
  
  luaunit.assertEquals(pane.focus,1)
  pane:setSelected(2)
  sleep(1)
  luaunit.assertEquals(pane.focus,2)
  pane:link(ui)
  mouseClick(ui,3,2)
  sleep(1)
  luaunit.assertEquals(pane.focus,1)
  mouseClick(ui,1,1)
  sleep(1)
  luaunit.assertEquals(pane.focus,2)
end

function test_replace()
  local g = Graphic:new("Item1")
  ui:add(g)
  ui:update()
  sleep(1)
  local h = Graphic:new("Item2")
  ui.pane:replace(g,h)
  --Warning, comparing graphic with reactor.parent causes java overflow
--  luaunit.assertEquals(ui.pane.index,{h})
  luaunit.assertEquals(ui.pane.content,{[h]=1})
  ui:update()
end

function tearDown_each()
  ui = nil
  console.restoreCursor()
end

print("EXIT: "..luaunit.LuaUnit.run())