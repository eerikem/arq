local EVE = {}

function EVE.queue(event,time)
  print("queuing event "..event.." in "..time)
end

colors={
 white=1,
 black=1,
 gray=1,
 lightGray=1,
 blue=1,
 lightBlue=1,
 yellow=1,
 orange=1,
 red=1,
 green=1,
 magenta=1,
 lime=1,
 pink=1,
 cyan=1,
 purple=1,
 brown=1,
}

local term = {
  w = 7, h = 5,
  cx_pos = 1, cy_pos = 1,
  text = "white",
  back = "black"
  }
  
local function emptyTerm()
  local contents = {}
  for i=1,term.h do
    contents[i]={}
    for j=1, term.w do
      contents[i][j] = {color = "black",text = "white",char= " "}
     end
  end 
  return contents
end

term.contents = emptyTerm()
term.window = emptyTerm()
term.redraw = function()
  term.contents = term.window
end

function term.write(str)
  for i=1, str:len() do
    if i>term.w then error(str.." too long for term at "..term.cx_pos,2) end
    term.window[term.cy_pos][term.cx_pos].char=str:sub(i,i)
    term.cx_pos = term.cx_pos + 1
  end
end

function term.getBackgroundColor()
  return term.text
end

function term.getTextColor()
  return term.text
end

function term.setTextColor(c)
  term.text = c
end

function term.setBackgroundColor(c)
  term.back = c
end

function term.clear()
  term.contents = emptyTerm()
end

local function dump() 
  for _,line in ipairs(term.contents) do
    local str = ""
    for _,row in ipairs(line) do
      str = str .. row.char
    end
    print(str)
  end
end

function term.setCursorPos(x,y)
  term.cx_pos=x
  term.cy_pos=y
end
function term.getCursorPos() 
  return term.cx_pos, term.cy_pos
end
function term.getSize()
  return term.w,term.h
end


--local luaunit = require 'lib.luaunit'
local UI = require 'lib.ui_lib'
local Panel = require 'lib.ui_obj'
--
local function start(co,w,h,init)
  init(UI:new(term))
end
--
local Graphic = require "lib.graphic"
--local body = Panel:new()
--local title = Graphic:new("Test UI")
--local button = Graphic:new("Button1")
--ui:add(title)
--button.ypos = 2
--body:add(button)
--ui:add(body)
--ui:update()

local telehub = require 'telehub_ui'
--local Teleporter = require 'telehub'
local name, UI_mod = debug.getupvalue(telehub.start,4)
UI_mod.start = start
debug.setupvalue(telehub.start,4,UI_mod)
telehub.start(nil,"Sect. E",nil)
dump()