# jstris-ai

Jstris-ai is a bot designed to play tetris on [Jstris](https://jstris.jezevec10.com/).
The program works by controlling Chrome via [Selenium](https://selenium.dev/).
The program requires Selenium version 2.53.1, which you can find [here](http://selenium-release.storage.googleapis.com/index.html).
You also need to install the Selenium Chrome Driver, which you can find [here](https://chromedriver.chromium.org/downloads).
I am testing the program against Chrome 78.

Note that the program is very brittle.
I've hard coded a lot of assumptions about the DOM of Jstris and how it renders.
These assumptions hold on my computer when I'm writing this program, but there are no guarantees that they will continue to hold on another computer, or in the future.
If you're trying to build this, good luck!

# Building

This program is built with [Stack](https://docs.haskellstack.org/en/stable/README/).
Once you have stack installed, simply run `stack build` from the project root to build the program.

# Running

1. First, start the Selenium server.
You can probably do this by just double clicking the jar file you downloaded above.
Alternatively, you can run it from the command line via `java -jar /path/to/selenium-server-standalone-2.53.1.jar`.
Note that Selenium needs to be able to find the Chrome Driver.
Make sure the Chrome Driver executable you downloaded is in your PATH.
1. Now, you should be ready to run the AI! Simply run `stack run` from the project root.
And that's that!


