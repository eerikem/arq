local gen_server = require "gen_server"
local Bundle = require "bundle"
local Panel = require "ui_obj"
local Menu = require "ui_menu"
local Group = require "group"

local CABLE_SIDE = "back"
local MONITOR = "monitor_5"
local cables = {
  override = Bundle:new(CABLE_SIDE,colors.yellow,"Override"),
  power = Bundle:new(CABLE_SIDE,colors.white,"Power"),
  fan_lights = Bundle:new(CABLE_SIDE,colors.magenta,"Fan Lights"),
  fans = Bundle:new(CABLE_SIDE,colors.orange,"Fans"),
  fan_door = Bundle:new(CABLE_SIDE,colors.lightBlue,"Fan Door"),
  flicker = Bundle:new(CABLE_SIDE,colors.lime,"Power Flicker"),
  loop = Bundle:new(CABLE_SIDE,colors.pink,"Loop")
}

local function initCables(cables)
  cables.power:enable()
  cables.fan_door:enable()
  cables.fan_lights:disable()
  cables.fans:disable()
  cables.flicker:disable()
  cables.loop:disable()
end

local cmds = {
  attack = "/playsound frontierdevelopment:event.event_attackmain @a[r=55] -183 46 -1501 500 1.0",
  torch = "/setblock -74 26 -17 50",
  explode = {
    "/summon PrimedTnt -74 31 -20",
    "/setblock -75 26 -21 51",
    "/setblock -72 26 -22 51",
    "/setblock -71 26 -21 51"
  },
  fall1 = {
    "/setblock -74 37 -21 13",
    "/setblock -71 37 -23 13"
  },
  fall2 = {
    "/setblock -72 37 -22 13",
    "/setblock -72 37 -24 13"
  },
  enderman = "/noppes clone spawn event_e3 1 -84,26,-23",
  rmTorch = "/setblock -74 26 -17 0",
  fanOn = "/spawnpoint @a -52 34 -12",
  explodeGas = "/summon PrimedTnt -56 28 -9",
  lightGas = {
    "/setblock -55 25 -4 51",
    "/spawnpoint @a -60 15 -10"
  },
  destroyFan = {
    "/summon PrimedTnt -56 20 -11",
    "/summon PrimedTnt -55 20 -9"
  },
  ledge = "/playsound frontierdevelopment:event.event_attackventfall @a[r=55] -183 46 -1501 500"
}

local fan = Group.group(-52,22,-14,-59,15,-7)
local ledge = Group.group(-59,32,-7,-52,32,-7)
local l2 = Group.group(-59,32,-14,-52,32,-14)
local l3 = Group.group(-59,32,-13,-59,32,-8)
local l4 = Group.group(-52,32,-13,-52,32,-8)
Group.combine(ledge,l2)
Group.combine(ledge,l3)
Group.combine(ledge,l4)

local NAME = "attack"

local function exec(cmd)
  if commands then
    if type(cmd) == "table" then
      for _,c in ipairs(cmd) do
        commands.execAsync(c)
      end
    else
      commands.execAsync(cmd)
    end
  else
    VM.log("Warning: Not a command computer")
  end
end

local function runAttack(State)
  local sum = 0
  local function wakeAt(time)
    time = time - sum
    EVE.sleep(time)
    sum = sum + time
  end
  VM.log("Running Attack Event")
  local cables = State.cables
  cables.power:disable()
  exec(cmds.attack)
  wakeAt(3)
  exec(cmds.torch)
  wakeAt(22)
  cables.power:enable()
  wakeAt(29)
  cables.power:disable()
  cables.flicker:enable()
  wakeAt(30)
  exec(cmds.explode)
  wakeAt(31)
  exec(cmds.fall1)
  wakeAt(32)
  exec(cmds.fall2)
  wakeAt(37)
  exec(cmds.enderman)
  wakeAt(40)
  cables.flicker:disable()
  exec(cmds.rmTorch)
  wakeAt(60)
  cables.power:enable()
  wakeAt(62)
  cables.fan_lights:enable()
  wakeAt(65)
  exec(cmds.fanOn)
  cables.fans:enable()
  wakeAt(69)
  exec(cmds.explodeGas)
  wakeAt(75)
  exec(cmds.lightGas)
  wakeAt(80)
  exec(cmds.destroyFan)
  cables.fans:disable()
  wakeAt(80.5)
  for _,block in ipairs(fan) do
    local str = string.format("/setblock %d %d %d 0",block.x,block.y,block.z)
    exec(str)
  end
  wakeAt(81)
  cables.loop:enable()
  wakeAt(82)
  exec(cmds.ledge)
  for _,block in ipairs(ledge) do
    local str = string.format("/setblock %d %d %d 13",block.x,block.y,block.z)
    exec(str)
  end
end

local function attackUI()
  local ui = ui_server.newWindow(MONITOR,18,5)
  ui:setBackground(colors.lightGray)
  ui:setText(colors.gray)
  local title = Graphic:new("Attack Control")
  title.align= "center"
  title.ypos = 2
  
  local doorButton = Graphic:new("Open Door")
  doorButton:setTextColor(colors.white)
  doorButton.xpos = 2
  local attackButton = Graphic:new("Launch Attack")
  local hiddenStatus = Graphic:new("         ")
  hiddenStatus.xpos = 2
  attackButton.xpos = 2
  attackButton.ypos = 2
  hiddenStatus.ypos = 3
  hiddenStatus:setTextColor(colors.red)
  local body = Panel:new()
  body.width = "max"
  body:setBackgroundColor(colors.gray)
  body:setTextColor(colors.lightGray)
  
  body:setLayout("static")
  body:add(doorButton)
  body:add(attackButton)
  body:add(hiddenStatus)
  ui:add(title)
  ui:add(body)
  
  ui:align("center")
  ui:update()
  
  local function buttonHandler()
    gen_server.cast(NAME,{"attack_button"})
  end
  attackButton:setOnSelect(ui,buttonHandler)
  
  local function doorHandler()
    gen_server.cast(NAME,{"door"})
  end
  doorButton:setOnSelect(ui,doorHandler)
  
  return ui,attackButton,hiddenStatus,doorButton
end

local Server = {}

function Server.start()
  Server.start_link()
end

function Server.start_link()
  return gen_server.start_link(Server,{},{},NAME)
end

local function enableButton(State)
  State.button:setTextColor(colors.green)
  State.enabled = true
end

function Server.init()
  local ui,button,status,doorButton = attackUI()
  local State = {ui = ui,button = button,enabled = false,status=status,cables=cables,open=false,door=doorButton}
  initCables(State.cables)
  EVE.subscribe("redstone",VM.running())
  if not State.cables.override:isOn() then
    enableButton(State)
    ui:update()
  end
  return true, State
end

function Server.handle_call(Request,From,State)
  gen_server.reply(From,"ok")
  return State
end

function Server.handle_cast(Request,State)
  local event = Request[1]
  if event == "redstone" then
    if State.enabled and State.cables.override:isOn() then
      State.button:setTextColor(colors.lightGray)
      State.enabled = false
    elseif not State.cables.override:isOn() then
      enableButton(State)
    end
    State.ui:update()
  elseif event == "attack_button" then
    if State.enabled then
      State.ui:ping()
      State.button:setTextColor(colors.lightGray)
      State.enabled = false
      State.ui:update()
      runAttack(State)
    else
      State.status.text = "Overide enabled"
      State.ui:beep()
      State.ui:update()
      EVE.sleep(1)
      State.status.text = "       "
      State.ui:update()
    end
  elseif event == "door" then
    if State.open then
      State.open = false
      State.door.text = "Open Door"
      State.cables.fan_door:enable()
    else
      State.open = true
      State.door.text = "Close Door"
      State.cables.fan_door:disable()
    end
    State.ui:update()
  end
  
  return State
end

return Server