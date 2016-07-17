local Group = {}

function Group.group(x,y,z,a,b,c)
  local T = {}
  local o,p,q
  if x <= a then o = 1 else o = -1 end
  if y <= b then p = 1 else p = -1 end
  if z <= c then q = 1 else q = -1 end
  for i = x,a,o do
    for n = y,b,p do
      for m = z,c,q do
        --writeStatus(i.." "..n.." "..m)
        table.insert(T,{x = i,y = n,z = m})
      end
    end
  end
  return T
end

function Group.insert(T,x,y,z)
  table.insert(T,{x = x,y = y,z = z})
end

function Group.combine(T,T2)
  for _,v in ipairs(T2) do
    table.insert(T,v)
  end
end

function Group.execute(T,cmd)
  for _,block in ipairs(T) do
    local str = string.format(cmd,block.x,block.y,block.z)
    exec(str)
  end
end

return Group