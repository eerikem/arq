--Product of ArqiTeknologies Corp.
--
--present
--
--Author: ArqiTek
--Copyright 2250

os.execute("cd P:/arq")
os.execute("cd")
io.write("->")

local funs = {}
local file = io.read()

local function fun()
	print "goodbye"
end

fun()

local printFile = function(file)
local f = assert(io.open(file))
print(f:read("*all"))
f:close()
end
