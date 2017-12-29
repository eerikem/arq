--Product of ArqiTeknologies Corp.
--
--Author: ArqiTek
--Copyright 2250
local gen_server = require "gen_server"
local ui_server = require "ui_server"
local PerphListener={}
local luaunit = require "luaunit"

local peripheralName = "toggler"

local function findPerph()
  return peripheral.find(peripheralName)
end

local function claimPerph(perph,State)
  VM.log("Claiming toggler")
  perph.setCoord(commands.getBlockPosition())
  State.perph = perph
  State.monitored = true
  return State
end

function PerphListener.start()
  return gen_server.start_link(PerphListener,{},{},"toggler")
end

function PerphListener.init()
  EVE.subscribe("toggler_attached")
  EVE.subscribe("toggler_detach")
  local perph = findPerph()
  if perph then
    return true, claimPerph(perph,{})
  end
  VM.log("WARNING: ARQ not paired with toggler")
  VM.log("  Add blockPerph to network to resolve")
  return true, {monitored = false}
end

function PerphListener.handle_cast(Request,State)
  local event = Request[1]
  if event == "toggler_attached" then
    local perph = findPerph()
    if perph then
      return claimPerph(perph,{})
    end
  elseif event == "toggler_detached" then
    --TODO handle toggler detach
    VM.log("Ignoring "..Request[2].." detachment")
  end
end

function PerphListener.handle_info(Request,State)
  VM.log("Warning handle info on toggler")
  return State
end


return PerphListener