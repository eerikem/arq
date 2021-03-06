local gen_server = require "gen_server"
local ui_server = require "ui_server"
local Door = require "door"
local Graphic = require "lib.graphic"
local Panel = require "lib.ui_obj"
local Server = {}
local luaunit = require "lib.luaunit"
local Menu = require "lib.ui_menu"

local checkup

function Server.start(doors)
  if not doors then
    error("Door manager requires doors",1)
  end
  local ok, Co = Server.start_link(doors)
  for _,door in ipairs(doors) do
    Server.add(Co,door)
  end
  checkup()
  return Co
end

function Server.open(manager)
  return gen_server.call(manager,{"open"})
end

function Server.close(manager)
  return gen_server.call(manager,{"close"})
end

function Server.lock(manager)
  return gen_server.call(manager,{"lock"})
end

function Server.unlock(manager)
  return gen_server.call(manager,{"unlock"})
end

function Server.add(manager,door)
  gen_server.cast(manager,{"add",door})
end

function Server.start_link(doors)
  return gen_server.start_link(Server,{doors},{})
end

local LOCKED = Graphic:new("locked")
local LOCKED_OPEN = Graphic:new("locked open")
local UNLOCKED = Graphic:new("unlocked")
local OPENED = Graphic:new("opened")
local CLOSED = Graphic:new("closed")
local FAKE = Graphic:new("fake")

FAKE:setTextColor(colors.lightGray)
LOCKED:setTextColor(colors.red)
LOCKED_OPEN:setTextColor(colors.red)
OPENED:setTextColor(colors.white)
CLOSED:setTextColor(colors.lightGray)

local OPEN = Graphic:new("Open")
local CLOSE = Graphic:new("Close")
local LOCK = Graphic:new("Lock")
local UNLOCK = Graphic:new("Unlock")

local function openHandler(manager)
  return function()
    Server.open(manager)
  end
end

local function closeHandler(manager)
  return function()
    Server.close(manager)
  end
end

local function lockHandler(manager)
  return function()
    Server.lock(manager)
  end
end

local function unlockHandler(manager)
  return function()
    Server.unlock(manager)
  end
end

local function managerUI(Co)
  local ui = ui_server.newWindow(Co,22,8)
  local title = Graphic:new("Access Control")
  title:align("center")
  title.ypos = 2
  local body = Panel:new()
  body:setLayout("static")
  body.width = "max"
  local menu = Menu:new()
  menu.width = 7
--  menu.ypos = 2
  menu.xpos = 2
  menu:link(ui)
  menu.width = "max"

  OPEN.xpos = 4
  CLOSE.xpos = 4
  LOCK.xpos = 12
  UNLOCK.xpos = 12

  local co = VM.running()
  OPEN:setOnSelect(ui,openHandler(co))
  CLOSE:setOnSelect(ui,closeHandler(co))
  LOCK:setOnSelect(ui,lockHandler(co))
  UNLOCK:setOnSelect(ui,unlockHandler(co))

  ui:add(title)
  ui:add(body)

  local buttons = Panel:new()
  buttons:setTextColor(colors.orange)
  buttons:setBackgroundColor(colors.lightGray)
  buttons:setLayout("static")
  buttons:add(OPEN)
  buttons:add(LOCK)
  buttons.ypos = 2
  ui:add(buttons)

  local indent = 9
  OPENED.xpos = indent
  LOCKED.xpos = indent
  CLOSED.xpos = indent
  LOCKED_OPEN.xpos = indent
  FAKE.xpos = indent
  
  body:add(menu)
  ui:setBackground(colors.lightGray)
  ui:setText(colors.gray)
  body:setBackgroundColor(colors.gray)
  body:setTextColor(colors.lightGray)

  ui:align("center","top")

  ui:update()
  checkup = function()
    VM.log("manager pane height: "..ui.pane.height)
    VM.log("manager body height: "..body.height)
    VM.log("manager menu height: "..menu.height)
    --    VM.log("check body getSize: "..table.concat({body:getSize(20)},", "))
    --    VM.log("body redraw "..body:redraw(ui))
  end
  return ui,menu,buttons
end

function Server.init()
  local ui,menu,buttons = managerUI("terminal")
  local State = {ui = ui,menu = menu,buttons=buttons,doors={},activeDoor=nil}
  return true, State
end

local changed = false

local function onDoorFocus(door,server)
  return function()
    gen_server.call(server,{"focus",door})
  end
end

local function addDoor(door,State)
  changed = true
  local title = Door.getTitle(door)
  VM.log("add door received "..title)
  local type = Door.getType(door)
  local open,locked = Door.getState(door)
  local item = Panel:new()
  State.doors[door] = item
  item:setLayout("static")
  item:add(Graphic:new(title))
  item.reactor:register("focus",onDoorFocus(door,VM.running()))
  if type == "fake" then
    item:add(FAKE)
  else
    if open then
      item:add(OPENED)
    elseif locked then
      item:add(LOCKED)
    else
      item:add(CLOSED)
    end
  end
  State.menu:add(item)
  Door.subscribe(door)
end

local function enable(panel,button,index)
  local i = index or 2
  local old = panel.index[i]
  if old == button then return end
  changed = true
  old.reactor:stop()
  button.reactor:start()
--  button:setTextColor(nil)
  panel:replace(old,button)
end

local function reenable(panel,button,index)
  local old = panel.index[index]
  if old ~= button then
    old.reactor:stop()
    panel:replace(old,button)
  end
  button:setTextColor(nil)
  button.reactor:start()
  changed = true
end

local function disable(panel,index)
  local obj = panel.index[index]
  changed = true
  obj:setTextColor(colors.gray)
  obj.reactor:stop()
end

local function updateButtonPanel(State,door)
  local doorState = State.doors[door].index[2]
  if doorState == FAKE then
    reenable(State.buttons,OPEN,1)
    reenable(State.buttons,LOCK,2)
    disable(State.buttons,1)
    disable(State.buttons,2)
    return
  end
  if doorState == CLOSED or doorState == LOCKED then
    reenable(State.buttons,OPEN,1)
  else
    reenable(State.buttons,CLOSE,1)
  end
  if doorState == LOCKED or doorState == LOCKED_OPEN then
    reenable(State.buttons,UNLOCK,2)
  else
    reenable(State.buttons,LOCK,2)
  end
end

function Server.handle_call(Request,From,State)
  local event = Request[1]
  if event =="focus" then
    local _,door = unpack(Request)
    updateButtonPanel(State,door)
    State.activeDoor = door
    gen_server.reply(From,"ok")
  elseif event == "close" then
    Door.forceClose(State.activeDoor)
    gen_server.reply(From,"ok")
  elseif event == "open" then
    Door.forceOpen(State.activeDoor)
    gen_server.reply(From,"ok")
  elseif event == "lock" then
    Door.lock(State.activeDoor)
    gen_server.reply(From,"ok")
  elseif event == "unlock" then
    Door.unlock(State.activeDoor)
    gen_server.reply(From,"ok")
  else
    VM.log("Door manager received: "..event)
    gen_server.reply(From,"ok")
  end
  if changed then
    changed = false
    State.ui:update()
  end
  return State
end

function Server.handle_cast(Request,State)
  local event = Request[1]
  if event == "add" then
    local _,door = unpack(Request)
    addDoor(door,State)
    if not State.activeDoor then
      State.activeDoor = door end
  elseif event == "opened" then
    local _,door = unpack(Request)
    if State.doors[door].index[2]==CLOSED then
      enable(State.doors[door],OPENED)
    else
      enable(State.doors[door],LOCKED_OPEN)
    end  
    if door == State.activeDoor then
      updateButtonPanel(State,door) end
  elseif event == "closed" then
    local _,door = unpack(Request)
    local doorState = State.doors[door].index[2]
    if doorState == OPENED then
      enable(State.doors[door],CLOSED)
    else
      enable(State.doors[door],LOCKED)
    end
    if door == State.activeDoor then
      updateButtonPanel(State,door) end
  elseif event == "locked" then
    local _,door = unpack(Request)
    local doorState = State.doors[door].index[2]
    if doorState == CLOSED or doorState == LOCKED_OPEN then
      enable(State.doors[door],LOCKED)
    else
      enable(State.doors[door],LOCKED_OPEN)
    end
    if door == State.activeDoor then
      updateButtonPanel(State,door) end
  elseif event == "unlocked" then
    local _,door = unpack(Request)
    local doorState = State.doors[door].index[2]
    if doorState == LOCKED then
      enable(State.doors[door],CLOSED)
    else
      enable(State.doors[door],OPENED)
    end
    if door == State.activeDoor then
      updateButtonPanel(State,door) end
  else
    VM.log("Door manager received: "..event)
  end
  if changed then
    State.ui:update()
    changed = false
  end
  return State
end

return Server
