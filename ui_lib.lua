local UI = {}

function UI:new(term)
  if not term then error("UI needs a term",2) end
  --setmetatable(self,{__index = term})
  local o = {pane = Panel:new(),term = term,reactor = Reactor:new()}
  setmetatable(o,self)
  self.__index = self
  
  o:registerUIListeners()
  
  return o
end

function UI:newWindow(x,y,w,h,visible)
  assert(x and y and w and h,'Four parameters expected')
  local t
  if not self.term then
    t = term.current()
  else
    t = self.term
  end
  local ui = UI:new(window.create(t,x,y,w,h,visible))
  ui.native = t
  ui:setBackground(t.getBackgroundColor())
  ui:setText(t.getTextColor())
  return ui
end

function UI:draw(obj)
  self.pane:applyColors(self)
  return obj:redraw(self)
end

function UI:add(...)
  local n = 0 
  for _,obj in ipairs(arg) do
    local o = self.pane:add(obj)
    if #self.pane.index > 1 then
    incCursorPos(self.term,1) end
    n = n + self:draw(o)
  end
  return n
end

function UI:remove(o)
  self.pane:remove(o)
end

function UI:printCentered(str, ypos,n,noscroll)
  if not n then n = 0 end
  local w,h = self.term.getSize()
  if w >= string.len(str) then
    return n + self:indentLeft(str,w/2 - #str/2,ypos,noscroll)
  else
    n = n + self:indentLeft(str,0,ypos,noscroll)
    return self:printCentered(string.sub(str,w+1),ypos+1,n,noscroll)
  end
end

--Indent relative to term
function UI:indentLeft(str, indent, ypos,noscroll)
  if ypos then
    self.term.setCursorPos(indent + 1, ypos)
  else
    local x,y = self.term.getPosition()
    self.term.setCursorPos(indent + 1, y)
  end
  return self:write(str,noscroll)
end

function UI:setBackground(color)
  self.pane.proto.background = color
end

function UI:setText(color)
  if type(self) == "number" then error("got it",2) end
  self.pane.proto.textColor = color
end

function UI:update()
--  local back = term.getBackgroundColor()
  --if self.pane.proto.background then
  --self.term.setBackgroundColor(self.pane.proto.background) end
  self.pane:applyColors(self)
  self.term.clear()
  self.term.setCursorPos(1,1)
  self:draw(self.pane)
  --term.setBackgroundColor(back)
end

function UI:align(...)
  local w,h = self.term.getSize()
  local x,y = self.term.getPosition()
  local px,py = self.native.getSize()
  local v={}
  if #arg > 2 then error('At most 2 args: "top","center" or "bottom","right"',2) end
  for _,pos in ipairs(arg) do
    v[pos]=true
  end
  if v["center"] then
    x = (px - w + 1) / 2
    y = (py - h + 1) / 2 end
  if v["bottom"] then
    y = py-h + 1 end
  if v["top"] then
    y = 1 end
  if v["right"] then
    x = px-w + 1 end
  if v["left"] then
    x = 1 end
  self.term.reposition(x,y)
end

function UI:write( sText,noscroll)
  if not sText then error("cannot write nil",2) end
  local w,h = self.term.getSize()    
  local x,y = self.term.getCursorPos()
  
  local nLinesPrinted = 0
  local function newLine()
    if y + 1 <= h then
      self.term.setCursorPos(1, y + 1)
    else
      if noscroll then
        return true
      else
        self.term.setCursorPos(1, h)
        self.term.scroll(1)
      end
    end
    x, y = self.term.getCursorPos()
    nLinesPrinted = nLinesPrinted + 1
  end
  
  -- Print the line with proper word wrapping
  while string.len(sText) > 0 do
    local whitespace = string.match( sText, "^[ \t]+" )
    if whitespace then
      -- Print whitespace
      self.term.write( whitespace )
      x,y = self.term.getCursorPos()
      sText = string.sub( sText, string.len(whitespace) + 1 )
    end
    
    local newline = string.match( sText, "^\n" )
    if newline then
      -- Print newlines
      if newLine() then return nLinesPrinted end
      sText = string.sub( sText, 2 )
    end
    
    local text = string.match( sText, "^[^ \t\n]+" )
    if text then
      sText = string.sub( sText, string.len(text) + 1 )
      if string.len(text) > w then
        -- Print a multiline word       
        while string.len( text ) > 0 do
          if x > w then
            if newLine() then return nLinesPrinted end
          end
          self.term.write( text )
          text = string.sub( text, (w-x) + 2 )
          x,y = self.term.getCursorPos()
        end
      else
        -- Print a word normally
        if x + string.len(text) - 1 > w then
          if newLine() then return nLinesPrinted end
        end
        self.term.write( text )
        x,y = self.term.getCursorPos()
      end
    end
  end
  
  return nLinesPrinted
end

function UI:register(obj,event)
  if event == "selected" then
    local handler = function (_x,y) self.pane.reactor:handleEvent("selected",y) end
    --todo already done in registerUIListeners
  elseif event == "scroll" then
    local handler = function (event,direction,x,y)
      local xpos,ypos = self.term.getPosition()
      local w,h = self.term.getSize()
      if x >= xpos and x < xpos + w then
        if y >= ypos and y < ypos + h then
          obj.reactor:handleEvent("scroll",direction)        
        end
      end 
    end
    self.reactor:register(event,handler)
  end
end

function UI:registerUIListeners()
  local event = "mouse_touch"
  local handler = function(_,x,y) self.pane.reactor:handleEvent("selected",y) end
  self.reactor:register(event,handler)
end

return UI