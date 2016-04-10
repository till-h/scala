A Simple Symbolic Calculator
============================

What it does
------------

Enter a valid mathematical expression made up from floating point numbers and a few standard operators and functions and this calculator evaluates it, returning the 
result.

The following characters / string sequences are accepted, with their conventional meaning. Trigonometric functions expect angles in radians.

    0-9 . () +-*/^ exp() ln() log() sin() cos() tan()

Testing
-------

There are two stages to testing. The first stage checks results from the calculator against manually obtained results. The second stage checks a different set of simple 
formulas against the output of the Google online calculator. To do this, the html code returned from the remote is scraped for the result.

Enjoy!
