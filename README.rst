Zombie Apocalypse
=================

Surviving a zombie apocalypse can be hard.

Wouldn't it be nice to know what factors are most important to your survival?

This is a class project for CSCI4800 and we've created a hell-scape where zombies chase people around.  We've used an inference engine created by our teacher to define the rules of the game.  After the walls are drawn in, the entire program is run with predicate logic searches.  There is a Gaussian distribution of speed and strength to make it interesting.

This was a chance to explore Racket and functional programming in general.  The most challenging part was imposing a system of "time" into the inference engine so every person/zombie took turns while moving and battling.  Using a loop that listens for events an controls the timing of the characters - like any video game - would make more sense, but "taming" an inference engine with a timer and a hierarchy of rules was an interesting challenge.

Authors
=======

| `James Brayton <https://github.com/jamesbrayton>`_
| `Andrew Hoyle <https://github.com/mettledrum>`_
| `Nic Young <https://github.com/nryoung>`_
