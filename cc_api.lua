
redstone = {}
rs = redstone
rs.getBundledInput = function() end
rs.getBundledOutput = function() end
rs.setBundledOutput = function() end

colors = {
    white="white",black="black",gray="gray",
    lightGray="lightGray", blue="blue",
    lightBlue="lightBlue", yellow="yellow",
    orange="orange",red="red",green="green",
    magenta="magenta",lime="lime",pink="pink",
    yan="cyan",purple="purple", brown="brown",    
    }

fs = {}

function fs.exists(name)
   local f=io.open(name,"r")
   if f~=nil then io.close(f) return true else return false end
end

function fs.open(...)
  local file = io.open(...)
  return {write = function(txt)
    file.write(txt)
  end,
  readAll = function()
    return file:read("*all")
  end,
  close = function()
    file:close()
  end
  }
end

rsBundle = {}

function resetBundle()
  for k,v in pairs(colors) do
    rsBundle[k] = false
  end
end
    
colors.combine = function(color,_) rsBundle[color]=true end
colors.subtract = function(_,color) rsBundle[color]=false end
colors.test = function(_,color) return rsBundle[color] end
