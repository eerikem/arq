
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
    --print("subscribing "..Request[2].." for "..Request[3]..".")
    HashArrayInsert(State,Request[3],Request[2])
    --print(type(State))
    --for K,Co in pairs(State) do print("smd"..K)end
  else
    --print("Received "..Request[1])
    --print(type(State))
    --for K,CO in pairs(State) do print("smd"..K)end
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
local Ui = Server.subscriber(Li,ui_sup)
ui_sup.app("terminal")

while true do
  gen_server.cast(Li,{os.pullEvent()})
end


return Server