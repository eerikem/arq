local luaunit = require 'luaunit'
local gen_server = require 'gen_server'
local Graphic = require 'graphic'
local ui_sup = require 'ui_supervisor'
local Panel, List = require 'ui_obj'
local Menu = require "ui_menu"

local Client = {}

function Client.start(Co)
  if not VM.registered("observer") then
    gen_server.start(Client,{Co},{},"observer")
  end
end

function Client.init(Co)
  Client.observerUI(Co)
  return true, {}
end

function Client.handle_cast(Request,State)
  local event = Request[1]
  if event == "die" then
    VM.exit("normal")
  else
    VM.log("observer got unkown event")
  end
  return State
end

local threads = {}
local names = {}
local ui

local function updateThreads()
  for name,thread in pairs(VM.coroutines) do
    if type(name) == "thread" and not names[thread] then
      names[thread] = string.sub(tostring(name),9)
    elseif type(name) == "string" then
      names[thread] = name
    end
  end
  for _,Name in pairs(names) do
    table.insert(threads,Name)
  end
  local hash2 = {}
  for thread,name in pairs(names) do
    hash2[name]=thread end
  return hash2
end

local function toNames(list)
  return map(function(thread)return names[thread]end,list)
end

local function threadToName(thread)
  
end

local function onThreadFocus(thread,pane)
  return function()
    pane.index[1].text = tostring(thread)
    if VM.co2names[thread] then
      pane.index[2].text = "Names: "..table.concat(VM.co2names[thread]," ")
    else pane.index[2].text = "Names: " end
    if names[VM.parents[thread]] then
      pane.index[3].text = "Parent: "..names[VM.parents[thread]]
    else
      pane.index[3].text = "Parent: Error!"
    end
    if VM.links[thread] then 
      pane.index[4].text = "Links: "..table.concat(toNames(VM.links[thread])," ")
    else pane.index[4].text = "Links:" end
    if VM.resumes[thread] then
      pane.index[5].text = "Resumes: "..VM.resumes[thread]
    else pane.index[5].text = "Resumes: Unkown" end
  end
end

function Client.observerUI(Co)  
  ui = ui_sup.newWindow(Co,40,15)
  ui:setBackground(colors.gray)
  ui:setText(colors.lightGray)
  local body = Panel:new()
  body:setLayout("static")
  body.width = "max"
  body:setBackgroundColor(colors.lightGray)
  body:setTextColor(colors.gray)
  
  local infoPanel = Panel:new()
  local info = List.fromArray({"","","","",""})
  infoPanel.width = 28
  infoPanel.height = 10
  infoPanel.xpos = 12
  infoPanel.ypos = 2
  infoPanel:setBackgroundColor(colors.gray)
  infoPanel:setTextColor(colors.lightGray)
  
  local t = Graphic:new("Observer")
  t.align="center"
  t:setBackgroundColor(colors.gray)
  t:setTextColor(colors.lightGray)
  
  local function kill()
    threads={}
    names={}
    gen_server.cast("observer",{"die"})
    ui = nil
  end
  
  t:setOnSelect(ui,kill)
  local nameToThread = updateThreads()
  local m = Menu.fromArray(threads)
  for _,item in ipairs(m.index) do
    local thread = nameToThread[item.text]
    item.reactor:register("focus",onThreadFocus(thread,info))
  end
  m.xpos = 2
  m.ypos = 1
  m:link(ui)
  
  ui:add(t)
  ui.term.reposition(1,1)
  body:add(m)
  infoPanel:add(info)
  body:add(infoPanel)
  ui:add(body)
  ui:update()
end

return Client