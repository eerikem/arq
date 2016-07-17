require = function(...)
  for _,file in ipairs(arg) do
    if fs.exists(file..".lua") then
      return dofile(file..".lua")
    elseif fs.exists("server/"..file..".lua") then
      return dofile("server/"..file..".lua")
    elseif fs.exists("arq/"..file..".lua") then
      return dofile("arq/"..file..".lua")
    elseif fs.exists("arq/src/"..file..".lua") then
      return dofile("arq/src/"..file..".lua")
    elseif fs.exists("arq/lib/"..file..".lua") then
      return dofile("arq/lib/"..file..".lua")
    else
      error("file not found: "..file)
    end
  end
end
