local gen_server = require 'gen_server'
local supervisor = require 'supervisor'
local Graphic = require 'lib.graphic'
local Menu = require 'lib.ui_menu'
local luaunit = require 'lib.luaunit'
local ui_sup = require 'ui_supervisor'
local Observer = require "observer"

local ArqMenu = {}

local function initUI()
  local ui = ui_sup.newWindow("terminal",15,7)
  local title = Graphic:new("ARQ Controls")
  
  local crash = Graphic:new("Kill Menu")
  local run = Graphic:new("Run Program")
  local observer = Graphic:new("Observer")
  local shutdown = Graphic:new("Shutdown")
  
  local menu = Menu:new()
  
  menu:add(crash)
  menu:add(run)
  menu:add(observer)
  menu:add(shutdown)
  
  title:setBackgroundColor(colors.lightGray)
  title:setTextColor(colors.gray)
  title:align("center")
  title.width = "max"
  title.ypos = 2
  
  menu.xpos = 2
  menu.width = "max"
  
  ui:add(title)
  ui:add(menu)
  
  menu:link(ui)
  
  menu:setBackgroundColor(colors.gray)
  menu:setTextColor(colors.lightGray)
  
  ui:setBackground(colors.lightGray)
  ui:setText(colors.gray)
  ui:align("center","left")
  VM.log("attaching crash to ui")
  crash.reactor:register("selected",ArqMenu.crash)
  shutdown.reactor:register("selected",function()os.queueEvent("terminate")end)
  observer.reactor:register("selected",function()Observer.start("terminal")end)
  ui:update()
  return ui
end


local Server = {}

function Server.start_link()
  return gen_server.start_link(Server,{},{})
end

function Server.init()
  VM.register("arq_menu",VM.running())
  return true, {ui = initUI()}
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
  return true, {{"one_for_one",1,0.05},{ChildSpec}}
end

function ArqMenu.start()
  supervisor.start_link(Sup,{},"arq_menu_sup")
end

function ArqMenu.crash()
  VM.log("ArqMenu crash called.")
  local ok, err = pcall(function()gen_server.cast("arq_menu","die")end)
  if not ok then VM.log(err) end
end

return ArqMenu
