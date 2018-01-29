
local JSON = require "lib.JSON"

local Config = {}

local CFG_FILE = ".arqconfig"

function Config.load()
  if fs.exists(CFG_FILE) then
    local fh = fs.open(CFG_FILE,"r")
    local rawConfig = fh.readAll()
    local cfg = JSON:decode(rawConfig) or {}
    fh.close()
    return cfg
  else
    local fh = fs.open(CFG_FILE,"w")
    fh.close()
    return {}
  end
end

function Config.save(data)
    local fh = fs.open(CFG_FILE,"w")
    local cfg = JSON:encode_pretty(data)
    fh.write(cfg)
    fh.close()  
end

return Config