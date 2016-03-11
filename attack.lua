local gen_server = require "gen_server"
local Bundle = require "bundle"
--0 seconds Play Sound,Disable White
--23 enable white
--31 Summon Entity
--36 Summon Entity
--67 Enable Yellow
--70 Summon Entity
local CABLE_SIDE = "back"
local cables = {
  override = Bundle:new(CABLE_SIDE,colors.black,"Override"),
  white = Bundle:new(CABLE_SIDE,colors.white,"White Signal"),
  yellow = Bundle:new(CABLE_SIDE,colors.yellow,"Yellow Signal")
}

local function attackUI()
  local ui = ui_server.newWindow("monitor_4",18,5)
  ui:setBackground(colors.lightGray)
  ui:setText(colors.gray)
  local title = Graphic:new("Attack Control")
  title.align= "center"
  
  local toggleButton = Graphic:new("Disable Lock")
  local attackButton = Graphic:new("Launch Attack")
  local hiddenStatus = Graphic:new("         ")
  hiddenStatus.xpos = 2
  
  local menu = Menu:new()
  menu.width = "max"
  menu.xpos = 2
  menu.ypos = 2
  menu:add(toggleButton)
  menu:add(attackButton)
  menu.proto.backgroundFocus = colors.gray
  menu.proto.textFocus = colors.white
  
  toggleButton.reactor:register("selected",function()
--     EVE.sleep(2)
--     ui:update()
     end)
  
  local body = Panel:new()
  body.width = "max"
  body:setBackgroundColor(colors.gray)
  body:setTextColor(colors.lightGray)
  
  body:add(menu)
  body:add(hiddenStatus)
  
  ui:add(title)
  ui:add(body)
  
  ui:align("center")
  ui:update()
  
  menu:link(ui)
  
  return ui,attackButton
end

local Server = {}

function Server.start()
  local Co = gen_server.start_link(Server,{},{},"attack")
  EVE.subscribe("redstone",Co)
end

function Server.init()
  local ui,button = attackUI()
  return {ui = ui,button = button,enabled = false}
end

function Server.handle_call(Request,From,State)
  gen_server.reply(From,"ok")
  return State
end

function Server.handle_cast(Request,State)
  local event = Request[1]
  if State.enabled then
    State.button:setTextColor(colors.lightGray)
    State.enabled = false
  else
    State.button:setTextColor(colors.green)
    State.enabled = true
  end
  State.ui:update()
  return State
end

return Server