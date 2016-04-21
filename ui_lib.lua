local UI = {}

local function exec(cmd)
  if commands then
    commands.execAsync(cmd)
  else
    VM.log("Warning: Not a command computer")
  end
end

local tapSound = "/playsound frontierdevelopment:event.buttonblip @p"
local selectSound = "/playsound frontierdevelopment:event.buttononc @p"
local errorSound = "/playsound frontierdevelopment:event.mondecline @p"

function UI:beep()
  exec(errorSound)
end

function UI:ping()
  exec(selectSound)
end

function UI:tap()
  exec(tapSound)
end

function UI:new(term)
  if not term then error("UI needs a term",2) end
  --setmetatable(self,{__index = term})
  local o = {pane = Panel:new(),term = term,selectables={},redraw = term.redraw}
  o.reactor = Reactor:new(o)
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
    self.pane.height = self.pane.height + n
    if #self.pane.index > 1 then
      self.pane.height = self.pane.height + 1
      n = n + 1
    end
  end
  return n
end

function UI:remove(o)
  self.pane:remove(o)
end

function UI:printCentered(str, ypos,n,noscroll)
  --TODO n is lines last printed, BAD DESIGN! 
  if not n then n = 0 end
  if type(n) == "boolean" then noscroll = n n = 0 end
  --TODO what is n doing here? Use below...
  --if n then error("what is n?",2) end
  local w,h = self.term.getSize()
  if w >= string.len(str) then
    return n + self:indentLeft(str,w/2 - #str/2,ypos,noscroll)
  else
    n = n + self:indentLeft(string.sub(str,1,w),0,ypos,noscroll)
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
  if not self.redraw then error("no redraw",2)end
--  VM.log("updating")
  return self:redraw()
  --term.setBackgroundColor(back)
end

function UI:update_sync()
  self.pane:applyColors(self)
  self.term.clear()
  self.term.setCursorPos(1,1)
  self:draw(self.pane)
  if not self.redraw_sync then error("no redraw",2)end
  return self:redraw_sync()
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
    x = math.floor((px - w + 1) / 2)
    y = math.floor((py - h + 1) / 2)
  end
  if v["bottom"] then
    y = py-h + 1 end
  if v["top"] then
    y = 1 end
  if v["right"] then
    x = px-w + 1 end
  if v["left"] then
    x = 1 end
  if x < 1 then x = 1 end
  if y < 1 then y = 1 end
  self.term.reposition(x,y)
  self.alignment = arg
end

function UI:write( sText,noscroll)
  if not sText then error("cannot write nil",2) end
  local w,h = self.term.getSize()    
  local x,y = self.term.getCursorPos()
  local indent = x
  local nLinesPrinted = 0
  local function newLine()
    if y + 1 <= h then
      self.term.setCursorPos(indent, y + 1)
    else
      if noscroll then
        return true
      else
        self.term.setCursorPos(indent, h)
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

function UI.lineWrap(sText,w,line,room)
  local line = line or ""
  local room = room or w
  local whitespace = string.match( sText, "^[ \t]+" )
  local x = 1
  if whitespace then
    -- Print whitespace
    if string.len(whitespace) > room then
      line = line .. string.sub(whitespace,1,room)
      return line,string.sub(sText,room + 1)
    else
      x = string.len(whitespace) + 1
      line = line..whitespace
      sText = string.sub( sText, x )
    end
  end
  local newline = string.match( sText, "^\n" )
  if newline then
    return line .. "\n", string.sub( sText, 2 )
  end
  
  local text = string.match( sText, "^[^ \t\n]+" )
  if text then
    if string.len(text) > w then
      -- Print a multiline word      
        return line .. string.sub(text,1,room),string.sub( sText, room + 1 )
    else
      -- Print a word normally
      if string.len(line) + string.len(text) > w then
        return line, sText
      end
    sText = string.sub( sText, string.len(text) + 1 )
    return UI.lineWrap(sText,w,line .. text,room - string.len(text))
    end
  end
  return line
end

function UI:onMe(x,y)
  local xpos,ypos = self.term.getPosition()
  local w,h = self.term.getSize()
  if x >= xpos and x < xpos + w then
    if y >= ypos and y < ypos + h then
      return true
    end
  end
  return false
end

function UI:register(obj,event)
  if event == "clickable" then
    self.selectables[obj]=true
  elseif event == "scroll" then
    local handler = function (event,direction,x,y)
      if self:onMe(x,y) then
        obj.reactor:handleEvent("scroll",direction)
      end
    end
    self.reactor:register(event,handler)
  elseif event == "keys" then
    local handler = function (event,code,down)
      if code == keys.enter
        or code == keys.left or code == keys.right
        or code == keys.up or code == keys.down then
        obj.reactor:handleEvent(event,code)
      end
    end
    self.reactor:register("key",handler)
  else
    error(event.." not a recognised ui interaction")
  end
end

function UI:relativeXY(x,y)
  local xpos,ypos = self.term.getPosition()
  y = y - ypos + 1
  x = x - xpos + 1
  return x,y
end

function UI:registerUIListeners()
  local clickDown
  local function clickHandler(event,id,button,x,y)
    if event == "mouse_click" then clickDown = id end
    if event == "mouse_up" and id ~= clickDown then
      return VM.log("Mouse up "..id.." from unrelated mouse down.")  
    end
    x,y = self:relativeXY(x,y)
--    VM.log("UI "..self.pane.id.." reactor got "..table.concat({id,button,x,y}," "))
    for obj,_ in pairs(self.selectables) do
      if obj:onMe(x,y) and obj.reactor.run then
        return obj.reactor:handleEvent(event,button,x,y)
      end
    end
  end
  
  local function touchHandler(event,x,y)
    x,y = self:relativeXY(x,y)
    for obj,_ in pairs(self.selectables) do
      if obj:onMe(x,y) and obj.reactor.run then
--        VM.log("touch on ui "..obj.id.." "..x.." "..y)
        return obj.reactor:handleEvent(event,x,y)
      else
--        VM.log("touch not on me")
      end
    end
  end
  
  self.reactor:register("mouse_click",clickHandler)
  self.reactor:register("mouse_up",clickHandler)
  self.reactor:register("monitor_touch",touchHandler)
end

return UI