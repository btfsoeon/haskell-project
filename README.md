# CSE 230 Project: Typeracer

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
