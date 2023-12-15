It's pretty hard to unit test this app - we can unit test the library functions but
that doesn't give us much signal on how the whole app performs


So we'll write some manual tests:

- [ ] Should have random text each time
- [ ] Cars move correctly
- [ ] Can type in characters and they appear on screen
- [ ] Backspacing works correctly
- [ ] Text color changes when typing correctly
- [ ] Text color changes to red when typing incorrectly
- [ ] Untyped text shows up as light blue
- [ ] WPM of CPU should be around 100
- [ ] WPM of player accurately represented, even when different characters like backspace
are typed
- [ ] Can hit enter to restart the game at the end
- [ ] Can hit escape at any time to exit game
- [ ] Can hit CTRL-C at any time to exit game
- [ ] Winner gets WINNER text above their car
- [ ] Countdown timer counts down 3-2-1
- [ ] CPU goes when countdown timer hits GO
