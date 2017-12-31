
local Server = {}
local appsDir = "apps"

function Server.start()
  local files = fs.list(appsDir)
  for _,appFile in ipairs(files) do
    local app = require(appFile)
    if app.start then
      app.start()
    else
      VM.log("App file "..appFile.." missing start()")
    end
  end
end

return Server