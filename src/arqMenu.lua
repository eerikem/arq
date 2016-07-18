local gen_server = require 'gen_server'
local supervisor = require 'supervisor'

local Server = {}

function Server.start_link()
  return gen_server.start_link(Server,{},{})
end

function Server.init()
  VM.register("arq_menu",VM.running())
  return true, {}
end

function Server.handle_call(Request,From,State)
  return State
end

function Server.handle_cast(Request,State)
  error("ArqMenu died :(")
  return State
end

function Server.terminate(Reason)

end

local ChildSpec = {"arq_menu",{Server,"start_link",{}}}

local Sup = {}

function Sup.init()
  VM.log("ArqMenu is Alive!")
  return true, {{"one_for_one",1,0.1},{ChildSpec}}
end

local ArqMenu = {}

function ArqMenu.start()
  supervisor.start_link(Sup,{},"arq_menu_sup")
end

function ArqMenu.crash()
  gen_server.cast("arq_menu","die")
end

return ArqMenu
