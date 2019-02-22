

local term = {
  w = 7, h = 5,
  cx_pos = 1, cy_pos = 1,
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

function term.write(str)
  for i=1, str:len() do
    term.contents[cy_pos][cx_pos].char=str:sub(i,i)
    cx_pos = cx_pos + 1
  end
end

function term.setCursorPos() end
function term.getSize() end


local luaunit = require 'lib.luaunit'
local UI = require 'lib.ui_lib'

local ui = UI:new(term)

