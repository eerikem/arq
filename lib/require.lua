local function checkFileExists(path)
  if fs.exists(path) and not fs.isDir(path) then 
    return true
  end
  return false
end

local require = function(name)
  if type(name) ~= "string" then
    error("Expected filename",2)
  end
  
  name = string.gsub(name,"%.lua$","")
  name = string.gsub(name,"%.","/")
  
  local ENV = getfenv(2)
  local loadedFiles = ENV.loadedFiles or {}
  ENV.loadedFiles = loadedFiles
  ENV.loading = ENV.loading or {}
  
  if ENV.loading[name] then
    error("Circular require! Already loading "..name,2)
  else
    ENV.loading[name]=true
  end
  
  local function stopLoading()
    ENV.loading[name] = nil
  end
  
  local dofile = function( _sFile )
    local fnFile, e = loadfile( _sFile )
    if fnFile then
      setfenv( fnFile, ENV )
      return fnFile()
    else
      error( e, 2 )
    end
  end
  
  local function loadFile(file)
--    print("loading "..file)
    local modules = {dofile(file)}
    loadedFiles[file] = {modules}
    stopLoading()
    return unpack(modules)
  end
  
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