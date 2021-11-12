# huarong-haskell
Final Project for CSE 230 Fall 2021

Chinese klotski puzzle
An implementation of the Chinese Klotski game in Haskell using the brick library.

The Chinese wooden game Huarong Dao, based on the ancient history from the Three Kingdom period, is a special Klotski puzzle that is popular in China. Like other sliding-block puzzles, several different sized wooden blocks are placed inside a box. Each brick can only move vertically or horizontally according to the available space left. The goal of the game is to use the minimal steps to move Cao Cao (the largest square one) to a special area designated by the game board (usually on the bottom). There are multiple settings of the game, and we will demonstrate this game using the classic setting called “Cha Chi Nan Fei” (Hard to escape) as shown in the picture. 

This application will render the game in command line. The player has 4 options towards each given brick, up “w”, down “s”, right “d”, and left “d”.During each round, the player would first select one brick then perform the movement.  Each movement can move the brick 1 unit on the board. If the brick cannot be moved anymore, none of those options would be executed if selected and will not be accounted for the total steps used. The optimal steps will be given on the top right corner as reference. 

The application will have an option to play with another player through networking. The two players will compete side by side and the player who solves the puzzle with fewer steps or lower time wins the game. The opponent’s board state will be shown on the side of the screen. 



<img src="https://upload.wikimedia.org/wikipedia/commons/thumb/a/a7/HuaRongDao.jpg/1024px-HuaRongDao.jpg" width="200"/>
<!-- [![Huarong Lane picture](https://upload.wikimedia.org/wikipedia/commons/thumb/a/a7/HuaRongDao.jpg/1024px-HuaRongDao.jpg)](https://upload.wikimedia.org/wikipedia/commons/thumb/a/a7/HuaRongDao.jpg/1024px-HuaRongDao.jpg) -->
Reference and image source:
[Wikipedia page for Klotski](https://en.wikipedia.org/wiki/Klotski)

Contributors: Dun Ma, Jiewen Yang, Mohan Li, Jinyi Mu
