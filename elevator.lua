local gen_server = require "gen_server"
local UI = require "ui"
local Graphic = require "graphic"
local Bundle = require "bundle"
local Radio = require "ui_radio_panel"
local ButtonPanel = require "ui_button_panel"
local Panel = require "ui_obj"

local CABLE_SIDE = "back"
local cables = {
  white = Bundle:new(CABLE_SIDE,colors.white,"White Signal"),
  yellow = Bundle:new(CABLE_SIDE,colors.yellow,"Yellow Signal")
}

local Elevator = {}

local function elevatorUI(Co,floor)
  local co, ui = UI.start(Co,7,5)
  
  local floors = Radio:new()
  local lvl1 = Graphic:new("1")
  local lvl2 = Graphic:new("2")
  local lvl3 = Graphic:new("3")
  
  lvl2.xpos = 3
  lvl3.xpos = 5
  
  floors:setLayout("static")
  floors:add(lvl1)
  floors:add(lvl2)
  floors:add(lvl3)
  floors:setSelected(floor)
  floors.align= "center"--TODO implement this
  floors.xpos = 2
  floors.proto.textFocus = colors.green
  
  local floorPanel = Panel:new()
  local lvl = Graphic:new("Level")
  lvl.align = "center"
  floorPanel:add(lvl)
  floorPanel:setTextColor(colors.green)
  floorPanel:add(floors)
  floorPanel.width = "max"
  local button1 = Graphic:new("1")
  local button2 = Graphic:new("2")
  local button3 = Graphic:new("3")
  
  button1.xpos = 2
  button1.ypos = 2
  button2.xpos = 4
  button2.ypos = 2
  button3.xpos = 6
  button3.ypos = 2
  
  local buttonPanel = ButtonPanel:new()
  
  local handler = function(button)
    return function ()
      local lvl = buttonPanel.content[button]
      VM.log("Button for "..lvl.." pressed!")
      buttonPanel:setSelected(lvl)
      ui:update()
      Elevator.callTo(lvl)
    end
  end
  
  local denied = Graphic:new("DENIED")
  denied.xpos = 2
  denied.ypos = 2
  denied:setTextColor(colors.red)
  local erroring = false
  local errorHandler = function(button)
    return function()
      if erroring then
        return end
      erroring = true
      local lvl = buttonPanel.content[button]
      VM.log("Button for "..lvl.." pressed!")
      ui:beep()
      buttonPanel:add(denied)
      ui:update()
      
      local fun = function()
        EVE.sleep(1)
        buttonPanel:remove(denied)
        ui:update()
        erroring = false
      end
      VM.spawn(fun)
    end
  end
  
  button1:setOnSelect(ui,handler(button1))
  button2:setOnSelect(ui,errorHandler(button2))
  button3:setOnSelect(ui,handler(button3))
  
  buttonPanel.width = "max"
  buttonPanel:setLayout("static")
  
  buttonPanel:add(button1)
  buttonPanel:add(button2)
  buttonPanel:add(button3)
  buttonPanel:noneSelected()
--  local buttonRow = Graphic:new("1 2 3")
--  buttonRow.xpos = 2
--  buttonRow.ypos = 2
--  buttonPanel:add(buttonRow)
  
  
  ui:add(floorPanel)
  ui:add(buttonPanel)
  
  local isDark = true
  local function dark()
    ui:setBackground(colors.gray)
    ui:setText(colors.lightGray)
    floors:setTextColor(colors.gray)
    floors.proto.backgroundFocus = colors.black
    floorPanel:setBackgroundColor(colors.black)
    buttonPanel.proto.backgroundFocus = colors.gray
    buttonPanel.proto.textFocus = colors.white
    isDark = true
  end
  local function bright()
    ui:setBackground(colors.lightGray)
    ui:setText(colors.gray)
    floors:setTextColor(colors.black)
    floors.proto.backgroundFocus = colors.gray
    floorPanel:setBackgroundColor(colors.gray)
    buttonPanel.proto.backgroundFocus = colors.lightGray
    buttonPanel.proto.textFocus = colors.white
    isDark = false
  end
  
  dark()
  
  ui:align("top")
  ui:update()
    
  local function colorHandler()
    if isDark then
      bright()
    else
      dark()
    end
    ui:update()
  end
  
  floorPanel:setOnSelect(ui,colorHandler)
    
  return ui,floors,buttonPanel
end

local function callPanelUI(Co,floor)
  local co,ui = UI.start(Co,7,5)
  
  ui:setBackground(colors.lightGray)
  ui:setText(colors.gray)
  
  local title = Graphic:new("Level "..floor)
  title.align = "center"
  
  local body = Panel:new()
  body:setLayout("static")
  body.width = "max"
    
  local callButton = Graphic:new("Call")
  callButton.xpos = 3
  callButton.ypos = 2 
  
  
  local called = false
  local function callReq()
    if not called then
      called = true
      ui:ping()
      callButton:setTextColor(colors.white)
      ui:update()
      Elevator.callTo(floor,co)
    end
  end
  body:setOnSelect(ui,callReq)
  
  body:add(callButton)
  body:setHeight(3)
  body:setBackgroundColor(colors.gray)
  body:setTextColor(colors.green)
  ui:add(title)
  ui:add(body)
  ui:update()
  
  local function handleOpened()
    called = false
    callButton:setTextColor(colors.green)
    ui:update()
  end
  
  UI.register(co,"opened",handleOpened)
  
  return ui, callButton
end

function Elevator.start()
  local Co = gen_server.start_link(Elevator,{},{},"elevator")
--  EVE.subscribe("redstone",Co)
end

function Elevator.init()
  local floor = 3
  local ui, floors, buttons = elevatorUI("monitor_1",floor)
  local ui2, call = callPanelUI("monitor_5",3)
  local ui3, call2 = callPanelUI("monitor_3",1)
  cables.white:disable()
  return true, {cables = cables,
          elevators = {{callPanels = {{ui = ui2,button = call},
                                     {ui = ui3,button = call2}},
                       floorPanels = {{ui = ui,floors = floors,buttons = buttons}},
                       floor = floor,
                       queue={},dest={},destButton={}}}}
end

function Elevator.handle_call(Request,From,State)
  gen_server.reply(From,"ok")
  return State
end

function Elevator.callTo(floor,button)
  if not floor then error("floor expected",2) end
  gen_server.cast("elevator",{"call",floor,button})
end

function Elevator.openDoor()
  gen_server.cast("elevator",{"openDoor"})
end
--
--function Elevator.moveTowards(dest)
--  gen_server.cast("elevator",{"move",dest})
--end

local function setFloor(lift,level)
  VM.log("Setting floor to "..level)
  for _,liftPanel in ipairs(lift.floorPanels) do
    liftPanel.floors:setSelected(level)
    liftPanel.ui:update()
  end
end

local function callToCurrentFloor(lift)
  lift.callPanels[1].ui.tap()
  lift.movingTo = lift.floor
  EVE.queue("openDoor",0.5)
end

local function openDoor(lift,State)
  State.cables.white:enable()
  lift.doorOpen = true
  --notify call button if applicable
  if State.button then
    UI.handleEvent(State.button,"opened")
    State.button = nil
  end
  --reset button panel
  local buttonPanel = lift.floorPanels[1]
  if lift.movingTo then
    buttonPanel.buttons:deselect(lift.movingTo)
    lift.movingTo=nil
  end
  buttonPanel.ui:update()
  
  State.ref = EVE.queue("closeDoor",4)
end

local function queueFloor(lift,dest,button)
  if not lift.dest[dest] then
    table.insert(lift.queue,dest)
    lift.dest[dest]=true
    if button then
      lift.destButton[dest]=button
    end
  end
end

local function callToOtherFloor(lift,dest)
  if lift.doorOpen then
    queueFloor(lift,dest)
    return
  end
  local buttonPanel = lift.floorPanels[1]
  lift.callPanels[1].ui.ping()--todo replace with intelligent ping
  lift.movingTo = dest  
  EVE.queue("move",0.5)
end

local function nextFloor(lift)
  local dest = lift.movingTo
  if dest < lift.floor then
    lift.floor = lift.floor - 1
    setFloor(lift,lift.floor)
  else
    lift.floor = lift.floor + 1
    setFloor(lift,lift.floor)
  end
  if lift.floor == dest then
    EVE.queue("openDoor",1)
  else
    EVE.queue("move",2)
  end
end

local function closeDoor(State)
    State.cables.white:disable()
    State.elevators[1].doorOpen = false
end

function Elevator.handle_cast(Request,State)
  local event = Request[1]
  if event == "call" then
    local dest = Request[2]
    local button = Request[3]
    VM.log("Floor "..dest.." called lift")
    local lift = State.elevators[1]
    --if moving queue request
    if lift.movingTo then
      if lift.movingTo ~= dest then
        queueFloor(lift,dest,button)
      else
        if button then
          State.button = button
        end
      end
    else
      State.button = button
      if dest == lift.floor then
        callToCurrentFloor(lift)
      else
        callToOtherFloor(lift,dest,State)
      end
    end
  elseif event == "move" then
    nextFloor(State.elevators[1])
  elseif event == "openDoor" then
    openDoor(State.elevators[1],State)
  elseif event == "closeDoor" then
    local lift = State.elevators[1]
    if State.ref == Request[2] then
      closeDoor(State)
      --TODO intelligent pulling from queue
      local dest = table.remove(lift.queue,1)
      if dest then
        lift.dest[dest] = nil
        Elevator.callTo(dest,lift.destButton[dest])
      end
    else
      VM.log("Waiting for next close door signal")
    end
  end
  return State
end

return Elevator