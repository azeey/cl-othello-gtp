Common Lisp GTP-Othello
=======================

GTP-Othello is a common lisp implementation of the GTP protocol. It
enables the use of GUIs such as Quarry to play a lisp implementation of
Othello with other Othello programs such as Grhino or Edax. It's not a
full blown implementation as it was written as a quick tool for an AI
class project. Thus, unless you are/were in the CS 260 class at
Vanderbilt University, it probably won't work for you straight out of
the box, but maybe a good starting point.

It has been tested to work on Linux, and should work on OSX, but no
guarantees. The implementation has been inspired by Grhino's and Edax's
GTP implementations.

Usage:
------

-   Open othello-gtp.lisp with your text editor and find the lines of
    code between the labels "START CHANGE" and "END CHANGE". Change the
    code to reflect your setup.
-   Make sure othello-gtp.sh has execute permission (Run "chmod +x
    othello-gtp.sh" if not sure)
-   Install and Run Quarry (Ubuntu: sudo apt-get install quarry)
    -   Navigate to Preferences -\> GTP Engines -\> Add
    -   Browse to where othell-gtp.sh is located and select it
    -   The command line text box should now the path. At the end of the
        command you can add an argument specifying the strategy to use.
        This would be a zero-based index into the *strategy* array of
        your othello.lisp
    -   Finish by clicking Add.

-   You can now add more gtp-engines or start a game. One good engine is
    Grhino (Ubuntu: sudo apt-get install grhino)

Video:
------
Here is a demo of using Quarry with othello-gtp <http://www.youtube.com/watch?v=Eof7rt7XSiQ>
