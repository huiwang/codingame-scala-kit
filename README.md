# CodinGame Scala Kit
I love both CodinGame and Scala because they make programming fun.

CodinGame is great and it would be better if I could
- Separate codes into packages and files
- Reuse existing codes
- Avoid tedious copy/paste
- Unit test the behavior of my bot
- Control source code versions with Git
- Stay in my favorite IDE [Intellij](https://www.jetbrains.com/idea/)

This kit achieves the above goals through a source code Bundler which assembles source codes from different packages and files into a single one.
Once we remove the constraint limiting us to code in just one file, we can organize our codes better and make them  reusable.

With continuous building/running feature in SBT, we can generate a uber file as soon as code is modified. 
Thanks to CodinGame Sync, the generated bundle can be synchronized to the online IDE automatically.

# How it works

## Pre-requisite
- Intellij
- Scala/SBT
- Git
- Chrome
- CodinGame Sync

## Step-by-Step Guide

1. Clone the _CodinGame Scala Kit_ with Git

    `git clone https://github.com/huiwang/CodinGame-Scala-Kit.git`

2. Import the SBT project in Intellij
3. Open a terminal and fire SBT, hit

    `~test` to compile and run unit tests continuously

4. Open a second terminal and fire SBT, hit

    `~runMain codingame.scala.bundle.Bundler` to bundling Player file continuously

5. Open CodinGame Sync to synchronize Player file continuously to online IDE

## Screenshot
![alt tag](./asset/screenshot.png)

# F&Q
1. How Bundler works?

    The Bundler reads a source file. Recursively, it replaces the import statements by source files found in this project.
    
    By default, the Bundler scans the `Player.scala` file from `src` folder and assemble all dependant source codes in a uber `Player.scala` file under `target` folder.
    
2. Can I reuse codes from third party?

    No, the Bundler only scans source files included in the project.   
    
# Next steps

- Create Local Arena to evaluate Bot offline
