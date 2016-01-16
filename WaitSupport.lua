-- This file implements waitSeconds, waitSignal, signal, and their supporting stuff.

-- This table is indexed by coroutine and simply contains the time at which the coroutine
-- should be woken up.
local WAITING_ON_TIME = {}

-- This table is indexed by signal and contains list of coroutines that are waiting
-- on a given signal
local WAITING_ON_SIGNAL = {}

local WAITING_ON_ANY = {}

--List of all coroutines their names, parent, children and supervisor.
local INDEX = {}
local NAME_INDEX= {}
--List of coroutine id's waiting on an event
local COROUTINES = {}

--List of coroutine hierarchies

local id = 0

local function printTable(_t)
  if _t == nil then
    writeStatus("nil table!")
  else
    for k,v in pairs(_t) do
      if type(v)=="thread" then
        writeStatus(INDEX[v].name)
      elseif type(v)=="table" then
        writeStatus(k.." contains")
        printTable(v)
        writeStatus("end "..k)
      elseif v==nil then
        v = "nil"
      else
        writeStatus(k.." "..v)
      end
    end
  end
end


-- Keep track of how long the game has been running.
--local CURRENT_TIME = 0

function waitSeconds(seconds)
  -- Grab a reference to the current running coroutine.
  local co = coroutine.running()

  -- If co is nil, that means we're on the main process, which isn't a coroutine and can't yield
  assert(co ~= nil, "The main thread cannot wait!")

  -- Store the coroutine and its wakeup time in the WAITING_ON_TIME table
  -- local wakeupTime = CURRENT_TIME + seconds
  local wakeupTime = os.startTimer(seconds)
  WAITING_ON_TIME[co] = wakeupTime


  -- And suspend the process
  return coroutine.yield(co)
end

local function delete(co) 
--OVERIDE
end

function stop(_co)
  writeStatus("stopping: "..INDEX[_co].name)
  if _co == nil then error("_co is nil", 2) end
  if INDEX[_co].parent == nil then error("the parent is nil",2) end
  if INDEX[INDEX[_co].parent] ~= nil then

    if INDEX[_co].parent ~=nil then
      local children = INDEX[INDEX[_co].parent].children

      if children.parent ~= "nil" then
        --remove from parent
        for n,c in ipairs(children) do
          if c==_co then table.remove(children,n) break end
        end
      else
        writeStatus("Warning: deleting main thread?")
      end

      delete(_co)
    end
  end
end

local function handler(co,ok,...)
  local c = INDEX[co]
  if not ok then
    if not c then error(NAME_INDEX[co].." no longer indexed!",2) end
    writeStatus(c.name.. " not ok")
    writeStatus(unpack(arg))
    if c.supervisor then
      coroutine.resume(c.supervisor,"error_msg",unpack(arg))
    else
      stop(co)
      error( unpack(arg),3)
    end
  else
    return unpack(arg)
  end
end


local function resume(co, ...)
  --print "restarting starting co"
  --writeStatus ("             "..INDEX[co].. " is1 "..coroutine.status(co))
  if coroutine.status(co) ~= "dead" or INDEX[co] ~= nil then
    return handler(co,coroutine.resume(co,unpack(arg)))
    --writeStatus ("             "..INDEX[co].. " is2 "..coroutine.status(co))
  else
    writeStatus("WARNING: attempt to resume dead coroutine "..NAME_INDEX[co])
    return true
  end
end

function wakerUpper()
  while true do
    local event, time = waitSignal("timer")
    --print ("waking all at "..time)
    local threadsToWake = {}
    for co, wakeupTime in pairs(WAITING_ON_TIME) do
      --print ("wakeupTime is "..wakeupTime)
      if wakeupTime == time then
        table.insert(threadsToWake, co)
      end
    end

    --print ("waking "..#threadsToWake.." threads")

    for _, co in ipairs(threadsToWake) do
      WAITING_ON_TIME[co] = nil
      resume(co)
    end
  end
end


function wakeUpWaitingThreads(deltaTime)
  -- This function should be called once per game logic update with the amount of time
  -- that has passed since it was last called
  CURRENT_TIME = CURRENT_TIME + deltaTime

  -- First, grab a list of the threads that need to be woken up. They'll need to be removed
  -- from the WAITING_ON_TIME table which we don't want to try and do while we're iterating
  -- through that table, hence the list.
  local threadsToWake = {}
  for co, wakeupTime in pairs(WAITING_ON_TIME) do
    if wakeupTime < CURRENT_TIME then
      table.insert(threadsToWake, co)
    end
  end

  -- Now wake them all up.
  for _, co in ipairs(threadsToWake) do
    WAITING_ON_TIME[co] = nil
    resume(co)
  end
end

function waitAny()
  local co = coroutine.running()
  --print (INDEX[co].. " wait for any")
  assert(co ~= nil, "The main thread cannot wait!")
  table.insert(WAITING_ON_ANY,co)
end

function stopWait()
  local _co = coroutine.running()
  for n, co in ipairs(WAITING_ON_ANY) do
    if co == _co then
      table.remove(WAITING_ON_ANY,n)
    end
  end
end

local function handleTerm(co,event,...)
  if event == "terminate" then
    stop(co)
  else
    return event, unpack(arg)
  end
end

function waitSignal(signalName)
  -- Same check as in waitSeconds; the main thread cannot wait
  local co = coroutine.running()
  assert(co ~= nil, "The main thread cannot wait!")

  COROUTINES[co] = signalName

  if WAITING_ON_SIGNAL[signalName] == nil then
    -- If there wasn't already a list for this signal, start a new one.
    WAITING_ON_SIGNAL[signalName] = { co }
  else
    table.insert(WAITING_ON_SIGNAL[signalName], co)
  end
  --printTable(WAITING_ON_SIGNAL)
  return handleTerm(co,coroutine.yield(co))
end


local function deleteChildren(_co)
  for _,co in ipairs(INDEX[_co].children) do
    delete(co)
  end
end

delete = function(_co)

  deleteChildren(_co)
  
  local signal = COROUTINES[_co]
  --if signal ~=nil then
  --writeStatus(INDEX[_co].." being stopped")

  if WAITING_ON_SIGNAL[signal] ~= nil then
    for n, co in ipairs(WAITING_ON_SIGNAL[signal]) do
      if _co == co then
        table.remove(WAITING_ON_SIGNAL[signal],n)
      end
    end
    if #WAITING_ON_SIGNAL[signal]==0 then
      WAITING_ON_SIGNAL[signal]=nil
    end
  end
  COROUTINES[_co]=nil
  INDEX[_co]=nil
end

local function signalWaitAny(signalName,...)
  for n, co in ipairs(WAITING_ON_ANY) do
    resume(co, signalName, unpack(arg))

    if coroutine.status(co)=="dead" then
      WAITING_ON_ANY[n] = nil
    end
  end
end

function signal(signalName, ...)

  local threads = WAITING_ON_SIGNAL[signalName]
  --writeStatus("received signal "..signalName)
  signalWaitAny(signalName, unpack(arg))

  local n = 0
  for _ in pairs(WAITING_ON_SIGNAL) do n = n + 1 end
  --writeStatus("There are "..n.." signals waiting")
  if threads == nil then return end
  --writeStatus ("threads waiting: " .. #threads)
  WAITING_ON_SIGNAL[signalName] = nil
  for _, co in ipairs(threads) do
    COROUTINES[co]= nil
    resume(co, signalName, unpack(arg))
  end
end

function runProcess(func,name,supervisor)
  -- This function is just a quick wrapper to start a coroutine.
  if not name then
    error("coroutine with no name",2) end
  local co = coroutine.create(func)


  local parent = coroutine.running()
  INDEX[co]={children={}}
  INDEX[co].parent = parent
  if #INDEX > 0 then
    table.insert(INDEX[parent].children,co)
  end
  INDEX[co].name = name .. "_"..id
  INDEX[co].supervisor = supervisor
  NAME_INDEX[co]=INDEX[co].name
  id = id + 1
  return resume(co)
end

function getWaitList()
  return WAITING_ON_SIGNAL
end
