local function checkFileExists(path)
  return fs.exists(path)
end

local require = function(name)
  local ENV = getfenv(2)
  local loadedFiles = ENV.loadedFiles or {}
  ENV.loadedFiles = loadedFiles
  
  local dofile = function( _sFile )
    local fnFile, e = loadfile( _sFile )
    if fnFile then
      setfenv( fnFile, getfenv(3) )
      return fnFile()
    else
      error( e, 2 )
    end
  end
  
  local function loadFile(file)
    local modules = {dofile(file)}
    loadedFiles[file] = {modules}
    return unpack(modules)
  end
  
  if not ENV.loadedFiles then
    loadedFiles = {} end
  local file = shell.resolve(name)
  if loadedFiles[file] then
    return unpack(ENV.loadedFiles[file])
  end
  if checkFileExists(file) then
    return loadFile(file)
  elseif checkFileExists(file..".lua") then
    return loadFile(file..".lua")
  else
    for _,path in ipairs(USR_PATH) do
      file = fs.combine(path,name)
      if loadedFiles[file] then
        return unpack(loadedFiles[file])
      end
      if checkFileExists(file) then
        return loadFile(file)
      elseif checkFileExists(file..".lua") then
        return loadFile(file..".lua")
      end
    end
  end
  error("module '"..name.."' not found",2)
end

return require