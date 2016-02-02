
local Server = {}
EVE = Server
VM = dofile("server/vm.lua")
UI = dofile("arq/ui.lua")
gen_server = dofile("server/gen_server.lua")
ui_server = dofile("arq/ui_server.lua")
ui_sup = dofile("arq/ui_supervisor.lua")

--VM = require 'vm'
--gen_server = require 'gen_server'
--ui_server = require 'ui_server'
--EVE = require 'eventListener'

--gen_server = require "gen_server.lua"


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
      --print("cororeaklj")
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

local Li = Server.start_link()
local Ui = Server.subscriber(Li,ui_sup)

while true do
  gen_server.cast(Li,{os.pullEvent()})
end


return Server