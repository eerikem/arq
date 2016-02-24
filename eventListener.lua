
local Server = {}
EVE = Server

VM = require "vm"
--UI = require "ui"
UI = require "ui_lib"
gen_server = require "gen_server"
ui_server = require "ui_server"
ui_sup = require "ui_supervisor"
Graphic = require "graphic"
Panel, List = require "ui_obj"
Menu = require "ui_menu"

function Server.start_link()
  return gen_server.start_link(Server,{},{})
end

function Server.init()
  return {}
end

function Server.handle_call(Request,From,State)
  return State
end

function Server.handle_cast(Request,State)
  if Request[1]=="subscribe" then
    HashArrayInsert(State,Request[3],Request[2])
  else
    if State[Request[1]] then
      for _,Co in ipairs(State[Request[1]]) do
        gen_server.cast(Co,Request)
      end
    end
  end
  --return "noreply", State
  return State
end

function Server.subscribe(Co,event)
  gen_server.cast(Co,{"subscribe",VM.running(),event})
end

function Server.subscriber(Co,Module)
  return Module.start_link(Co)
end

VM.init()
local Li = Server.start_link()
VM.register("events",Li)
local Ui = Server.subscriber(Li,ui_sup)
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


return Server