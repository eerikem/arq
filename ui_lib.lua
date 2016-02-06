local UI = {}

function UI:new(term)
  if not term then error("UI needs a term",2) end
  --setmetatable(self,{__index = term})
  local o = {pane = Panel:new(),term = term}
  setmetatable(o,self)
  self.__index = self
  return o
end

function UI:draw(obj)
  return obj:redraw(self)
end

function UI:add(obj)
  self.pane:add(obj)
  return self:draw(obj)
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

function UI:indentLeft(str, indent, ypos,noscroll)
  self.term.setCursorPos(indent + 1, ypos)
  return self:write(str,noscroll)
end

function UI:setBackground(color)
  self.pane.background = color
end

function UI:setText(color)
  if type(self) == "number" then error("got it",2) end
  self.pane.textColor = color
end

function UI:update()
  local back = term.getBackgroundColor()
  self.term.setBackgroundColor(self.pane.background)
  self.term.clear()
  self.term.setCursorPos(1,1)
  self:draw(self.pane)
  term.setBackgroundColor(back)
end

function UI:align(...)
  local w,h = self.term.getSize()
  local x,y = self.term.getPosition()
  local px,py = self.parent.getSize()
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

return UI