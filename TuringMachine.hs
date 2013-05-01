import Data.Maybe
import Data.Tuple

--give the static definition for a turing machine
--transition function goes from (state, tape symbol) to (state, tape symbol, direction of motion)
data TuringMachine = TuringMachine
 {accept::[Int], reject::[Int], transitionFunction::[( (Int, Char), (Int, Char, Bool) )]}

--give defintion for a running turing machine
--defined by a static machine along with the current state
data RunningTM = RunningTM {tm::TuringMachine, tape::[Char], state::Int, tapePosition::Int, halted::Bool}

--move the head forward or backwards
--direction of true means forwards
moveHead:: Int -> Bool -> Int
moveHead a b = if b
	       	  then
			a + 1
		  else
			a - 1

--redefine tuple accessors for 3-tuples
frst3 (a, _, _) = a
scnd3 (_, a, _) = a
thrd3 (_, _, a) = a

--see if current state is in the accept or reject list
checkHalt:: Int -> [Int] -> [Int] -> Bool
checkHalt state accept reject = elem state accept || elem state reject

--recursively advance a turing machine to the next state
transition:: RunningTM -> RunningTM
transition (RunningTM (TuringMachine accept reject transitionFunction) tape state tapePosition halted) = 
	   let
		--look up the transition function
		next = lookup (state, tape!!tapePosition) transitionFunction
	   in
		if (isNothing next)
		   then
			--if the transition function maps to nothing, halt
			(RunningTM (TuringMachine accept reject transitionFunction) tape state tapePosition True)
		   else
			let
				--pull the actual tuple out of the Maybe
				actualNext = fromJust(next)
			in
				--actually advance to the next tm state
				(RunningTM (TuringMachine accept reject transitionFunction) (concat [(take tapePosition tape), [scnd3 actualNext], (drop (tapePosition + 1) tape) ]) (frst3 actualNext) (moveHead tapePosition (thrd3 actualNext)) (checkHalt (frst3 actualNext) accept reject))

--call the recursive advancement function on a new turing machine with string to be decided
decide:: TuringMachine -> [Char] -> Bool
decide tm string = 
       runTM(RunningTM tm (concat[('#' : string),  ['_','_'..]]) 0 1 False)

--recursively call self, advancing the tm each time
runTM:: RunningTM -> Bool
runTM tm =
      let
	(RunningTM (TuringMachine accept reject transitionFunction) tape state tapePosition halted) = (transition tm)
      in
	--if machine stops, see if in accepting state
	if halted
	then
	   elem state accept
	else
	   --if not halted, call again
	   runTM (RunningTM (TuringMachine accept reject transitionFunction) tape state tapePosition halted)

main:: IO()

main = 
     let
	--define an example turing machine
	tm = (TuringMachine [2] [] [((0,'a'), (1,'_',True)), ((1,'a'), (0,'_',True)), ((0,'_'), (2,'_',True))])
     in
	let
	   --define sigma star
	   sigmaStar = [c : s | s <- "" : sigmaStar, c <- ['a']]
	in
	   do
		putStrLn "This demonstrates the use of a Turing machine. The following is sigma star over an alphabet of {a}:"
		putStrLn (show(take 20 sigmaStar))
		putStrLn "This is what's accepted by a turing machine for the language of even length strings:"
		putStrLn (show(take 10 (filter (decide tm) sigmaStar)))