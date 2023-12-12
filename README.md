<div align="center">
<pre>
  _   _      _             _             
 | | (_)    | |           | |            
 | |_ _  ___| |_ __ _  ___| |_ ___   ___ 
 | __| |/ __| __/ _` |/ __| __/ _ \ / _ \
 | |_| | (__| || (_| | (__| || (_) |  __/
  \__|_|\___|\__\__,_|\___|\__\___/ \___|

-----------------------------------------
A tictactoe CLI game where you play 
against a computer player with a minimax 
algorithm implementation.
</pre>

![GitHub top language](https://img.shields.io/github/languages/top/ItsThompson/tictactoe)
![GitHub last commit (branch)](https://img.shields.io/github/last-commit/ItsThompson/tictactoe/main)
![GitHub code size in bytes](https://img.shields.io/github/languages/code-size/ItsThompson/tictactoe)

</div>

## Introduction
I was inspired to build a tic-tac-toe AI solver from one of the exercise sets for Programming 1 (CM12003) at University of Bath. My main objectives for this project were to deepen my understanding of algorithms for (AI) and gain practical experience in functional programming/Haskell. When researching algorithms, I stumbled across the [Minimax algorithm](https://course.elementsofai.com/2/3). This algorithm is designed specifically for two-person, perfect information, zero-sum games like Tic-Tac-Toe. By systematically evaluating potential moves, assuming players play the best move and maximize their chance of winning, the Minimax algorithm selects the most advantageous move by considering all possible outcomes.

## Showcase

https://github.com/ItsThompson/tictactoe/assets/33135895/83b3b2b0-d739-432e-a6d6-48ce26e7aab7

## Build Instructions 
Using a Haskell compiler, such as GHC, you can compile the code to a standalone executable. (Source: [Haskell Wiki](https://wiki.haskell.org/Haskell_in_5_steps))
```
ghc -o tictactoe main.hs
```

## Usage
### Running in Glasgow Haskell Compiler's interative environment (GHCi)
There are two ways to run the program inside the interative environment. You can call the `main` function:
```
$ ghci main.hs
ghci> main
```
Alternatively you can also call the `tictactoe` function:
```
$ ghci main.hs
ghci> tictactoe 
```

### Running the Executable
If you compiled the code into a executable, you can directly run the executable (`./tictactoe` on Unix systems, `tictactoe.exe` on Windows).
```
$ ./tictactoe
```
## Reflection
There are definetly better and more efficient ways of approaching this question. For example, during the problem lecture, Professor Willem Heijltjes explained the optimal move can be determined by simply checking if you are currently winning and if the opponent has no winning move. With this method, assuming optimal play, it will always result in a draw. However, the advantage of a Minimax algorithm is the notion of how it can expand to other zero-sum games such as Chess, Monopoly, and even card games.

### Further Investigations
It would be interesting to explore a Minimax implementation for different cardgames such as poker or blackjack because the winstate is more complicated than tic-tac-toe.
