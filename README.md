Conway's Game of Life in Idris/SDL
----------------------------------

This is a very basic implementation of Conway's Game of Life in Idris with
SDL GUI.

Building instructions
---------------------

To compile this, you'll need Edwin Brady's SDL bindigns for Idris, available
here: https://github.com/edwinb/SDL-idris.

*WARNING*: The bindings seem to be abandoned right now and do not compile with
Idris 1.3.0. However, there's a fixed version available here:
https://github.com/beefyhalo/SDL-idris

Having installed SDL bindigns, issue the following command:

    $ idris --ibcsubdir $PWD/build --build life.ipkg

An executable named `life` will appear in the root directory of the repository,
Run it and have fun!

Usage
-----

I believe the interface is quite intuitive, but just to make sure:

* Press [Esc] to exit.
* Press an arrow key to scroll the view.
* Press n to go to the next generation.
* Press i to zoom in.
* Press o to zoom out.
* Press c to clear the game state.
* Press s to dump the board state to stdout.
* Click a cell to switch it between alive and dead state.

More options will be added in the future!
