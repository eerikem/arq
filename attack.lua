local attack = {}
root = attack
local title = 'Attack Control'
local uis={}
attack.uis = uis

local override = false;
local CABLE_SIDE = "back"

local collection = group.group(-95,37,-9,-99,37,-5)
group.insert(collection,-99,36,-7)
group.insert(collection,-99,35,-7)

local cables = {
  override = BUNDLE:new(CABLE_SIDE,colors.yellow,"Override"),
  player = BUNDLE:new(CABLE_SIDE,colors.white,"PlayerDetector"),
  orange = BUNDLE:new(CABLE_SIDE,colors.orange,"Orange"),
  magenta = BUNDLE:new(CABLE_SIDE,colors.magenta,"MagentaPulse"),
  lightBlue = BUNDLE:new(CABLE_SIDE,colors.lightBlue,"LightBluePulse"),
  lime = BUNDLE:new(CABLE_SIDE,colors.lime,"Lime"),
  flicker = BUNDLE:new(CABLE_SIDE,colors.pink,"Flickering")
  }
  
local function define(_time, _desc, _cmd)
  return {time = _time, description = _desc, command = _cmd}
end

local events = {
  define(0, "Play Sound", "/playsound frontierdevelopment:event.event_attackmain @a[r=55] -183 46 -1501 500 1.0 1"),
  define(0, "Place Block", "/setblock -101 36 -7 0"),
  define(0, "Set Spawnpoint", "/spawnpoint @a -97 14 -7"),
  define(1, "Disable Redstone", function() cables.orange:disable() end),
  define(1, "Place Block", "/setblock -100 34 -7 875 3"),
  define(7, "Redstone Pulse", function() cables.magenta:pulse() end),
  define(14, "Effect", "/effect @a 9 15"),
  define(15, "Redstone Flicker", function() cables.flicker:enable() end),
  define(19, "Redstone Pulse", function() cables.lightBlue:pulse() end),
  define(19, "Command", "/noppes clone spawn attack_mainfaller 2 -98,44,-6"),
  define(20, "Command", "/noppes clone spawn attack_mainfaller 2 -99,43,-8"),
  define(22, "Command", "/noppes clone spawn attack_mainfaller 2 -96,45,-8"),
  define(24, "Command", "/noppes clone spawn attack_mainfaller 2 -97,43,-7"),
  define(26, "Command", "/noppes clone spawn attack_mainfaller 2 -98,44,-6"),
  define(26, "Disable Redstone Flicker", function() cables.flicker:disable() end),
  define(28, "Place Block", "/setblock -100 34 -7 0"),
  define(35, "Setblock Array", function() group.execute(collection,"/setblock %d %d %d 13") end),
  define(40, "Enable Redstone", function() cables.lime:enable() end)
}

local function launchAttack()
  local time = 0
  for n,event in ipairs(events) do
    if event.time - time > 0 then
      waitSeconds(event.time - time)
      time = event.time
    end
    writeStatus("launchAttack->"..event.description)
    if(type(event.command)=="string") then
      exec(event.command)
    elseif type(event.command) == "function" then
      event.command()
    else
      error("unknown event type!")
    end
  end
end

local function detector()
  while true do
    local event = waitSignal("redstone")
    if event == "terminate" then return end
    if cables.player:isOn() then
      if not cables.override:isOn() then
      return runProcess(launchAttack,"AttackEvent")
      else
        writeStatus("Cannot launch attack. No player detected.")
      end
    else
      writeStatus("Cannot launch attack. Override is on!")
    end
  end
end

attack.init = function()
  rs.setBundledOutput(CABLE_SIDE,0)
  cables.orange:enable()
  table.insert(uis,ui)
end

attack.main = function()
  return runProcess(detector,"attackDetector")
end