--Product of ArqiTeknologies Corp.
--
--Author: ArqiTek
--Copyright 2250

local args = {...}


--print = function() end
--write = function() end
--sleep = function() end

VM = require "vm"
--UI = require "ui"
Reactor = require "reactor"
UI = require "ui_lib"
gen_server = require "gen_server"
ui_server = require "ui_server"
ui_sup = require "ui_supervisor"
EVE = require "eventListener"
Graphic = require "graphic"
Panel, List = require "ui_obj"
Menu = require "ui_menu"


VM.init()
local Li = EVE.start_link()
--local Ui = EVE.subscriber(Li,ui_sup)
local Ui = ui_sup.start_link(Li)
local write = ui_sup.statusWindow("terminal")
VM.log = write
ui_sup.app("terminal")
local write2 = ui_sup.statusWindow("monitor_4")
write2("I am here")
--write("hello1")
--write("another...rea;u;u asdgmajs dg asd gjkla;sdjkg;jalskdg askdjlg; asdj gsdj kgl;a sjklasdgj ;askdj g;alskdj gl;asjdkg ")
--write("error: this is an error error error error error error error error error error error2")
--write("hello2")
--write("hello3")
--write("hello4")
--write("another...rea;u;u asdgmajs dg asd gjkla;sdjkg;jalskdg askdjlg; asdj gsdj kgl;a sjklasdgj ;askdj g;alskdj gl;asjdkg ")
--write("error: this is an error error error error error error error error error error error2")

--scroll("up")
--scroll("up")
--scroll("up")
--write("hello5")

while true do
  --VM.flush()
  gen_server.cast(Li,{os.pullEvent()})
end