local UI = {}

function UI:new(term)
  if not term then error("UI needs a term",2) end
  setmetatable(self,{__index = term})
  local o = {}
  setmetatable(o,self)
  self.__index = self
  return o
end

--
--function UI:new(o)
--  local o = o or {}
--  setmetatable(o, self)
--  self.__index = self
--  return o
--end

function UI:draw(obj)
  local old = term.redirect(self)
  obj:redraw()
  term.redirect(old)
end

return UI