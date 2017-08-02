[![Build Status](https://travis-ci.org/TrueLaurel/CodinGame-Scala-Kit.svg?branch=master)](https://travis-ci.org/TrueLaurel/CodinGame-Scala-Kit)
[![Gitter](https://badges.gitter.im/TrueLaurel/CodinGame-Scala-Kit.svg)](https://gitter.im/TrueLaurel/CodinGame-Scala-Kit?utm_source=badge&utm_medium=badge&utm_campaign=pr-badge)

# What's new?
> 2017-07 [Bot Architecture](http://truelaurel.com/2017/07/07/Bot-Architecture-in-CodinGame-Scala-Kit/)
>
> 2017-06 [Design Evaluation Function](http://truelaurel.com/2017/05/01/Design-Evaluation-Function/)
>
> 2017-04 [Debugging in CodinGame Scala Kit](http://truelaurel.com/2017/04/27/Debug-in-CodinGame-Scala-Kit/)

# Resources


# CodinGame Scala Kit
I love Both CodinGame and Scala because they make programming fun.

CodinGame is great and it would be better if I could
- Separate codes into packages and files
- Reuse existing codes
- Generate the required Player file automatically
- Avoid tedious inter-IDE copy/paste
- Unit test the behavior of my Bot
- Control source code versions with Git
- Stay in my favorite IDE [Intellij](https://www.jetbrains.com/idea/)

This kit achieves these goals through a source code Bundler which assembles source codes from different packages and files into a single one.
Once we remove the constraint imposing us to code in one file, we can organize codes better and make them more reusable.

With continuous building/running feature in SBT, we can generate the fat Player file as soon as code is modified. 
Thanks to CodinGame Sync, the generated bundle can be synchronized to the online IDE automatically.

# Example Bot
The example Bot [Ghost in the Cell](https://www.codingame.com/multiplayer/bot-programming/ghost-in-the-cell) ranked `59/3509` overall and `2/50` for Scala language.

# Setting Up

## Pre-requisite
- Intellij
- Scala/SBT
- Git
- Chrome
- CodinGame Sync

## Step-by-Step Guide

1. Clone the _CodinGame Scala Kit_ with Git

    `git clone https://github.com/truelaurel/CodinGame-Scala-Kit.git`

2. Import the SBT project in Intellij
3. Open a terminal and fire SBT, hit

    `~test` to compile and run unit tests continuously

4. Open a second terminal and fire SBT, hit

    `~runMain com.truelaurel.codingame.tool.bundle.BundlerMain GhostInTheCell.scala` to bundle destination file continuously

5. Open CodinGame Sync to synchronize Player.scala file continuously to online IDE

## Screenshot
![alt tag](./asset/screenshot.png)



# FAQ
1. How Bundler works?

    The Bundler reads a source file. Recursively, it replaces the import statements by source files found in this project.
    
    By default, the Bundler scans the `Player.scala` file from `src` folder and assembles all dependant source codes in a uber `Player.scala` file under `target` folder.
    
2. Can I reuse codes from third party?

    No, the Bundler only scans source files included in the project.

3. Is Java Supported?

    No, the Bundler only inlines imported source codes and doesn't adapt Java code to Scala. 
    If you prefer Java, I strongly recommend Manwe's great [Competitive Programming](https://github.com/Manwe56/competitive-programming) tools.
    
# More questions?
Let's discuss it in the [CodinGame forum](https://www.codingame.com/forum/t/codingame-scala-kit/2645/1)!
