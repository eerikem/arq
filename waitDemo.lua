-- This file contains the actual demonstration code. The stuff that implements waits and signals
-- is in WaitSupport.lua

dofile("WaitSupport.lua")
--require "luasocket"

host = "raw.githubusercontent.com"
file = "/eerikem/arq/master/menu"



function setText1(s)
	print(s)
end
function setText2(s)
	print(s)
end

function George()
    while true do
        setText1("Hey there, Bob.")
        waitSeconds(2)
        signal("go-ahead-bob-1")
        waitSeconds(0.5)
        setText1("")
        waitSignal("go-ahead-george-1")
        setText1("Can't complain. It's been raining. You?")
        waitSeconds(2)
        signal("go-ahead-bob-2")
        waitSeconds(0.5)
        setText1("")
        waitSignal("go-ahead-george-2")
        setText1("You too, Bob.")
        waitSeconds(2)
        setText1("")
        waitSeconds(2)
    end
end

function Bob()
    while true do
        waitSignal("go-ahead-bob-1")
        setText2("Hi George. How's it going?")
        waitSeconds(2)
        signal("go-ahead-george-1")
        waitSeconds(0.5)
        setText2("")
        waitSignal("go-ahead-bob-2")
        setText2("Same old, same old.")
        waitSeconds(2)
        setText2("Looking forward to some snow.")
        waitSeconds(2)
        setText2("Well, I best be going. Take care, George.")
        waitSeconds(2)
        signal("go-ahead-george-2")
        waitSeconds(0.5)
        setText2("")
    end
end

runProcess(George)
runProcess(Bob)
