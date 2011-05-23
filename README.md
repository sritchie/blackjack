# Functional Blackjack #

This project contains all code necessary to play an epic game of
functional blackjack.

## Running Instructions ##

NOTE: The project uses a text interface, and must be run in a unix
terminal.

To run from source, build an uberjar of the project by `cd`ing into
the project directory and running

    cake uberjar

or

    lein uberjar

(I've also attached an uberjar of the current project to my email.)

To run the game, all that's needed is:

    java -jar blackjack-standalone.jar

### Running from Code ###

    $ lein repl
    backtype.blackjack.core=> (-main)


