# CSE 230 Project: Typeracer

Cameron Trando, Alex Park

# Introduction

As computer scientists and software engineers, it's not unreasonable to say many of us are competitive. It would also be fair to say that most of us have spent a great deal of time at our computers. When you mix these two traits together, you get programming competitions and mathlathons, but more importantly, you get typing races, better known as [typeracer](https://play.typeracer.com/). 

The premise of the game is simple. You gather your friends together and you want to determine who's the most dextrous - who has spent the most time at a computer cranking away at their keyboards. You race each other on some random text you've picked out, taking care to precisely type your apostrophes and hyphens, furiously backspacing to correct your mistakes. 

In the end, the fastest (not necessarily the most accurate) typist is determined the winner, and they can take pride in their specialized ability. 

# Project Proposal

So immediately after hearing about creating a project involving a CLI, we thought to recreate typeracer, an online game where you can challenge your friend to see who types the fastest. Typeracer simulates a race, where contestants prepare to type out some randomly selected text of around 50-100 words, and whoever types out that text the fastest wins. If contestants make a mistake while typing out the text, they cannot continue until they rectify it. 

At the end, each competitor is presented with a summary of their average words a minute when typing the text as well as their accuracy. 


## Goals

In order to help shape our final game, we'll list out our goals.

### Fundamentals 

Fundamentally, the rules of type racer are that the user is presented with a paragraph of text and has to type it out. If they make a mistake, they cannot continue until they have corrected it. The current character the user has to type should be highlighted in order to let them know where they are in the text.

The user has 5 seconds to view the text before they must begin typing, and they cannot begin typing until after a timer has begun (similar to the start timer in Mario Kart or any other racing game).

There is no time limit on how long the user takes to complete the paragraph, but we should cap this off at something reasonable like 10 minutes. The race is only over for a player after they finish typing. If one player finishes typing the text first, then the race is not over for them until they finish typing the text.

At the end of the race, we should present to each client the average words per minute of every competitor and present the accuracy of each client to themselves.


### Large Corpus of Random Texts

We must provide a large selection of random texts to sample from. It's no fun if users have to repeatedly type the same text over and over, and it fails to uphold the spirit of the game; to test one's typing speed. 

Each text should have an author field, so after the player has completed typing the text, they can see where the text is from in case it's familiar to them. 

We should strive to provide a rich selection of texts from various books and/or pop culture. 


### Networked Multiplayer 

The online version of type racer is a multiplayer game with up to 8 players. For our purposes, we'll strive to achieve at least a two player maximum, mostly because our group is only two players. 

This means that our application must be able to tell which of the players finished typing the text first, and the two clients must be in constant communication with each other, or some third party server, in order to maintain this state. Furthermore, we'll have to synchronize the two clients together to make sure they're typing the same text and start at the same time. 


### UI Quirks

The original type racer game comes with a nice graphic of cars going along a road. Every player represents a car. The closer you are to completing the text, the further and further your car travels along the road, eventually hitting the end. This is a nice visual indicator of telling players how far their opponents are and if they should try harder to catch up. We would like to create a similar graphic. Our technical skills may be limited in how complex this graphic should be, but it seems reasonable that we would be able to visually represent the progress of the other players to each client.

This will again, require some more networking work to keep this state synchronized. 


## Stretch Goals

We'll also present some stretch goals if we managed to finish our original goal early.

### More detailed typing breakdown

The real typeracer game gives a breakdown of how fast the player was during each segment of the text. For example, it breaks down a text into sections of 10 words each and tells the user how accurate and how fast they were during each segment. This can help someone realize what characters or words are problematic and work to improve their time in the future. 


# Setup for client

You'll need this config set in your `~/.stack/config.yaml` file:
```
allow-newer: true
```

Then just run `stack install` or `stack build`. 


# Setup for server

Run `stack install` and then `typeracer-server -p 8000`. 


# Project Update

## What is the architecture of your application (the key components)?

The architecture of our app is just a simple client server application. One client acts as the server and talks to the other client. The idea is that one client will determine the time to start and the text to be typed out and send it to the other client. Then the other client will be constantly POSTing its progress and words per minute and GETing the progress and words per minute of the server.

Then each client will have the progress of the other participant as well as the same text and start time. In that way, we can sycnhronize the two to have them start at relatively the same time. To break ties, we'll have the client with the higher words per minute be the winner.

That's the networking part of it.

On the client side, we have an OK UI where we show the text and a car graphic that will move along the page as you make more progress on the text. We have a timer that counts down and won't let you type until the counter hits 0. We'll support two cars on the screen to represent you and your opponentn, and we'll show the words per minute of both.


## What challenges (if any) did you have so far and how did you solve them?

The main challenge we've had is working with Haskell. I've worked with many languages before, but I have never encountered a language with such poor package management support. Packages are poorly documented (typing information is not enough!), and there's a lot of conflicting information on whether we should use `stack` or `cabal` or both. It's difficult to force the package manager to pick a specific package version (sometimes cabal overrides stack, or the `package.yaml` overrides both, etc.). This could be unfamiliarity with the language, but upon further research it seems like a very common issue that language infrastructure is poor to abysmal. 

The only IDE with decent support is vscode, and compiler errors are obscure and strange because of the haskell language server tries to infer types and can get it wrong and give questionable output. 

We haven't really been able to solve these. It doesn't get easier, but hopefully we'll get better.

In terms of actual implementation, I hit a snag where I needed a game loop. I wanted a counter to count down from 5 to 1, but we would only handle events on keyboard or mouse events. There was no timer implemented.

So I had to make a timer myself, forking a thread and having it run every 33 milliseconds to simulate 30 fps, firing a new BrickEvent every time the thread ran. In practice, this should have been easy, but Brick had pretty sparse docs on this, particularly on BChan, so it was pretty difficult for me to get this working. I had to infer a lot of behavior based on what I knew about blocking channels from Golang.


## Do you expect to meet your goals until the deadline?

I think we should still be able to hit our goals. Maybe the UI won't look as nice as I thought, but at minimum we should be able to get working multiplayer. Once we have that, a lot of the work will be cosmetic improvements. 

## If not, how will you modify your goals?

We'll limit the scope of our cosmetic improvements. We absolutely need that multiplayer implementation though. If it really comes down to it, we'll limit development on that and fully improve cosmetic features intead, but I hope it doesn't come to that.


# Credits

We bootstrapped using the [gotta-go-fast](https://github.com/callum-oakley/gotta-go-fast) library, which has the BSD-3 license so our usage is in accordance with that.

## What we bootstrapped

* We borrowed logic in setting up the application - i.e. arg handling. We did modify that logic to have our own args, but the initial set up came from that library.
* We borrowed the initial schema of our State datatype and the logic on updating that state when typing characters. We updated the datatype significantly, but, again the setup came from that library.
* We borrowed the rendering and wrapping of the initial selected text and made minor adjustments to this code
* We borrowed the framework for determining typing accuracy i.e. Hits/Misses/Empty


## What we added

* We changed how the initial random text was selected and made it correctly handle newlines
* We changed the file format for how texts would be stored so we could parse them easily
* We added an "animated" countdown timer to start the game
* We added another CPU player that would race against you
* We added a way to detect and display the winner of the game
* We added a game loop that would update the game state periodically
* We added a words per minute graphic per player that would update as they typed
* We added a car and road graphic per player 
* We moved each car along their respectives road as players would get further along the text
* We added sensible replay functionality 
* We distinguished between letters yet to be typed/letters typed/letters typed incorrectly and colored them brightly
to help out players

As an aside, we had no intention of plagiarizing. The credits section above was present before the due date of the project. We transparently acknowledged we bootstrapped off an existing project, but we felt our projects were sufficiently different. We're deeply sorry if that was not the case or if that was not allowed.
