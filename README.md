> "Robust, practical and fast" Clojure is aiming at giving developers "succinctness, flexibility and productivity" (dixit [http://clojure.org/about/rationale](the originator) of this language). I've been choosing it for its succinctness as one can express more and write less. I've tried to avoid endless cogitation and get things done (that's to say: stay focused).

Hence the name **POC**: proof of concept, project on chess and perhaps practice of concentration.

You can find gentle introductions to related topics in the [](bibliography).

# Another game of Chess

__Salient features__ (links point to technical insights):

 * [](Functionnal programming): Clojure language;
 * [](Logic programming): miniKanren-based library `core.logic`;
 * [](Artificial intelligence): minmax algorithm based on a metric;
 * [](Context-free grammars): parser with the library `instaparse`;
 * [](Graphical interface): Unity game engine through Arcadia;
 * [](Focused on things done): can't do a lot but try to di it well.

__How much novel from [https://github.com/search?p=2&q=chess+logic&ref=searchresults&type=Repositories&utf8=%E2%9C%93](existing projects)?__

 * Only one (aborted) [https://github.com/matlux/clojure-core-logic-chess](other Chess project) using Clojure and core.logic;
 * Only one [https://github.com/Arunothia/ILP](other project) using logic programming.

__How much the same for me?__

 * I've had my first project as pet one in Clojure in 2015 July, this is my second try;
 * I've learned bits of automata theory for the aforementionned project;

This is merely a quick-sketched pet project hence performances are not that great — just on getting things done. I believe I can still do a lot to wrangle with the famous Chess combinatorial explosion.

__Can it be improved?__ Yes it can be improved in several ways for this is merely a POC (proof-of-concept or proof-of-concentration ^_^):

 * Use multimethods for parser (but it may become less straight-forward to get);
 * Entrust the input and stop verifying every moves (but what if User is Devil?);
 * Improve speed (yes sure! some steps in this draft may overlap hense be avoided).

## Installation

I'm ArchLinux user. These instructions are for reference only and you may adapt them depending on the operating system you have. I use Emacs but you might feel more comfortable with Light Table (all Clojure-written IDE), Cursive Clojure (by JetBrains) or even Eclipse and so forth.

 * First, upgrade your system (for example with `yaourt -Syyua`)
 * Install dependency manager `maven` 3 (`yaourt -S maven`)
 * Get `leiningen` installed on your computer (with the `AUR`: `yaourt -S leiningen`)
 * Install Clojure (`yaourt -S clojure`)
 * Install Emacs (advised piece of software anyway) (`yaourt emacs` then make a choice)
 * You should also take a look at [http://overtone.github.io/emacs-live/](Emacs live), at least for the spirit is has ;-)

Once you feel like you're done and have any tool you'd need, clone this repository:

```shell
cd some/where
git clone https://github.com/piotr-yuxuan/PoC/
```

You may like to keep following ongoing steps:

 * Hop in the freshly-cloned repository PoC (`cd PoC/`);
 * Compile and install the project (`lein repl install`);
 * If you like your terminal enough, launch a REPL and start exploring (`lein repl`)
 
Few piece of advice to help you if you want to use Emacs (and Emacs-live) to get hands in the code: open a Clojure file (`*.clj`) and press `C-c M-j` to start jack a new REPL in, then go back in the file frame and hit `C-c M-n` to change the REPL working namespace; finally load the file with `C-c C-k`. Piece of cake :-) Other key bindings are available at [https://github.com/clojure-emacs/cider#using-cider-mode](this paragraph).

## Usage

FIXME: explanation

    $ java -jar firstshot-0.1.0-standalone.jar [args]

## Options

FIXME: listing of options this app accepts.

## Examples

...

## Known bugs

...

## Any Other Sections
## That You Think
## Might be Useful

## License

Copyright © 2016 胡雨軒

Distributed under the General Public License either version 3.0 or (at your option) any later version.
