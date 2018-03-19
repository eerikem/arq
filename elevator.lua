local gen_server = require "gen_server"
local ui_server = require "ui_server"
local UI = require "lib.ui"
local Graphic = require "lib.graphic"
local Panel = require "lib.ui_obj"
local Password = require "password"
local Door = require "door"
local elvator_panel = require "elevator_panel"
local elvator_call = require "elevator_call"

local Elevator = {}

----------------
--External API--
----------------

--{{name=floorName,
--level=floor#,
--coord={x,y,z,r},
--door=doorCo,
--call=monitor,
--panel=monitor,
--}}

function Elevator.newCall(monitor,title,number,elevator,password)
  return elvator_call.start_link(monitor,title,number,elevator,Elevator,password)
end

function Elevator.newPanel(monitor,floor,elevator)
  return elvator_panel.start_link(monitor,floor,elevator,Elevator)
end

function Elevator.new(levels)
  return Elevator.start_link(levels)
end

function Elevator.subscribe(Co)

end

function Elevator.callTo(Co,lvl)
  gen_server.cast(Co,{"call",lvl})
end

function Elevator.start_link(levels)
  gen_server.start_link(Elevator,{levels},{})
end

function Elevator.init(levels)
  local activeLevel = 1
  -- organise levels into hash of levels
  -- sorted by level number
  local byLevel = {}
  for n,l in ipairs(levels) do
    l.waiting = false
    byLevel[l.level]=l
    activeLevel = l.level
    if l.call ~= nil then
      Elevator.newCall(l.call,l.name,l.level,VM.running(),l.password)
    end
  end
  return true, {
    activeLevel = activeLevel,
    destQueue = {},
    direction = nil,
    delay = 3,
    levels = byLevel,
    }
end

local function openDoor(State,level)
  if State.levels[level] then
    Door.open(State.levels[level].door)
  end
end

local function setDir(State,level)
  if State.activeLevel > level then
    State.direction = "down"
  else
    State.direction = "up"
  end
end

function Elevator.handle_cast(Request,State)
  local event = Request[1]
  if event == "call" then
    local level = Request[2]
    VM.log("Received call from level "..level)
    if State.activeLevel == level then
      openDoor(State,level)
    else
      if State.levels[level] == nil then
        VM.log("Not a valid destination: level "..level)
        return State end
      if not State.levels[level].waiting then
        local lvl = State.levels[level]
        lvl.waiting = true
        Door.close(lvl.door)
        if not State.direction then
          setDir(State,level)
          EVE.tick(State.delay)
        else
          table.insert(State.destQueue,level)
        end
      end
    end
  elseif event == "openDoor" then
    local level = Request[2]
  elseif event == "closeDoor" then
    local level = Request[2]
    if State.levels[level] then
      Door.close(State.levels.door)
    end
  end
  return State
end

function Elevator.handle_info(Request,State)
  if event == "wake" then
    VM.log("Received wake signal")
  else
    VM.log("Elevator got: "..unpack(Request))
  end
  return State
end

return Elevator