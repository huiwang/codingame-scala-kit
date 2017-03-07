# CodinGame Scala Kit
I love both CodinGame and Scala because they make programming fun.

CodinGame is great and it would be better if I could
- Code in my favorite IDE Intellij
- Separate codes into packages and files
- Reuse existing codes
- Unit test the behavior of my bot
- Avoid tedious repeating copy/paste
- Control source code versions with Git

That's why I created this Kit to make life easier. Hopefully, it can help you code better bot.

This kit helps you to program better bot.

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

    The Bundler reads a source file. Recursively, it replaces the import statements by source files found in this project
    
2. Can I reuse codes from third party?

    No, the Bundler only scan source files included in the project.
    
3. Is Java supported?
    
    I didn't test it. If you are interested in Java or C++, I strongly recommend Manwe's [Competitive Programming](https://github.com/Manwe56/competitive-programming) tools.
    
# Next steps

- Create Local Arena to evaluate Bot offline
