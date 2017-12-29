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

local function claimPerph(perph)
  VM.log("Claiming toggler")
  perph.setCoord(commands.getBlockPosition())
end

function PerphListener.start()
  return gen_server.start_link(PerphListener,{},{},"toggler")
end

function PerphListener.init()
  local perph = findPerph()
  if perph then
    claimPerph(perph)
    return true, {perph = perph,monitored = true}
  end
  VM.log("WARNING: Failed to aquire toggler!")
  return true, {monitored = false}
end


return PerphListener