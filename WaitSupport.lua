-- This file implements waitSeconds, waitSignal, signal, and their supporting stuff.

-- This table is indexed by coroutine and simply contains the time at which the coroutine
-- should be woken up.
local WAITING_ON_TIME = {}

-- This table is indexed by signal and contains list of coroutines that are waiting
-- on a given signal
local WAITING_ON_SIGNAL = {}

-- Keep track of how long the game has been running.
--local CURRENT_TIME = 0

function waitSeconds(seconds)
    -- Grab a reference to the current running coroutine.
    local co = coroutine.running()

    -- If co is nil, that means we're on the main process, which isn't a coroutine and can't yield
    assert(co ~= nil, "The main thread cannot wait!")

    -- Store the coroutine and its wakeup time in the WAITING_ON_TIME table
    --local wakeupTime = CURRENT_TIME + seconds
    local wakeupTime = os.startTimer(seconds)
    WAITING_ON_TIME[co] = wakeupTime
    
    
    -- And suspend the process
    return coroutine.yield(co)
end

local function resume(co, ...)
  --print "restarting starting co"
  local ok, param = coroutine.resume(co,unpack(arg))
  if not ok then
    print "received error"
    error( param )
  else
    return param
  end
end

function wakerUpper()
  while true do
    local event, time = os.pullEvent("timer")
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
        WAITING_ON_TIME[co] = nil -- Setting a field to nil removes it from the table
        resume(co)
    end
end


function waitSignal(signalName)
    -- Same check as in waitSeconds; the main thread cannot wait
    local co = coroutine.running()
    assert(co ~= nil, "The main thread cannot wait!")

    if WAITING_ON_SIGNAL[signalName] == nil then
        -- If there wasn't already a list for this signal, start a new one.
        WAITING_ON_SIGNAL[signalName] = { co }
    else
        table.insert(WAITING_ON_SIGNAL[signalName], co)
    end
    --printTable(WAITING_ON_SIGNAL)
    return coroutine.yield()
end

function signal(signalName, ...)
    local threads = WAITING_ON_SIGNAL[signalName]
    print ("received signal "..signalName)
    printTable(WAITING_ON_SIGNAL)
    sleep(1)
    if threads == nil then return end
    --print ("threads waiting: " .. #threads)
    WAITING_ON_SIGNAL[signalName] = nil
    for _, co in ipairs(threads) do
        resume(co, signalName, unpack(arg))
    end
end

function runProcess(func)
    -- This function is just a quick wrapper to start a coroutine.
    local co = coroutine.create(func)
    return resume(co)
end

function getWaitList()
  return WAITING_ON_SIGNAL
end
