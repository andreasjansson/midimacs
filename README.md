<img width="288" height="223" align="right" src="http://github.com/andreasjansson/midimacs/blob/master/github-assets/midiaccord.gif" />

Midimacs
========

A semi-algorithmic MIDI sequencer in Emacs Lisp.

Status
------

Currently very much alpha-quality. I've only tested it on Emacs 24.3 on Ubuntu 14.04. More of a proof of concept than a useful music making tool at the moment. Since it depends on amidicat it only runs on Linux. You'll need to know a bit of Emacs Lisp to be able to use it, it's not exactly user-friendly yet.

Installation
------------

1. Install [amidicat](http://krellan.com/amidicat/)
2. Install [cask](https://github.com/cask/cask)
3. Clone the midimacs repository
4. `cask install`
5. `(require 'midimacs)`

Tutorial
--------

`M-x midimacs` to start midimacs.

![](https://github.com/andreasjansson/midimacs/blob/master/github-assets/start.gif)

The top row is the timeline, it's read-only. The rest of the buffer is free text, you can type whatever you want. We'll call this buffer the "sequencer".

The numbers in the timeline represents beats, and each cursor position is one beat.

If a line starts with `>00 `, where `00` can be any two-digit number, it means that line will be a midi channel, all other lines are comments. A bit like literal programming!

![](https://github.com/andreasjansson/midimacs/blob/master/github-assets/line-basics.gif)

A midi channel line has a sequence of codes. A code is represented by an ascii character. It can be any character except period and space. Period means continue with the previous code (without initialization, we'll get to what that means in a second). Space means "no code".

In the gif above, channel `00` has the code `a` repeated four beats, then re-initialized, then repeated another four beats. In channel `01`, `b` is repeated three times, `c` times, and `d` three times. Everything afterwards is silent.

A code is actually a tiny little algorithm. If you hit RETURN while point is on a code, you'll be taken to that code's buffer, where you'll write the algorithm.

![](https://github.com/andreasjansson/midimacs/blob/master/github-assets/open-code.gif)

Most codes have two functions (well, macros actually, but think of them as functions): `midimacs-init` and `midimacs-run`.

`midimacs-init` is executed the first tick of every beat where the code is initialized, i.e. when the code is a character and not a period. It takes parameters
* _channel_ - the MIDI channel
* _song-time_ - the current position in the song, as a `midimacs-time` struct (see below)
* _length_ - the length of code sequence until it either re-initializes or stops
* _state_ - the current state of the channel (more below)

It returns the new channel state, I'll explain that in a bit.

Below is an example of the parameter passed in.

Here are a couple important commands: To evaluate the code buffer, hit `C-c C-c`. If you don't do that, nothing will change no matter how much you type. To start playing your piece, hit `M-SPC`. Same key to stop.

![](https://github.com/andreasjansson/midimacs/blob/master/github-assets/init-params.gif)

`midimacs-run` is executed every tick as long as there's a code in the sequencer. There are 24 ticks per beat.

It takes the same parameters as `midimacs-init`, except `length` is replaced by `rel-time` which is the time since the last initialization.

To illustrate `midimacs-run` I'll talk a bit about `midimacs-time` first. A `midimacs-time` struct has two fields, `beat` and `tick`. A number of arithmetic functions are defined for `midimacs-time`, like `midimacs-time+`, `midimacs-time-max`, `midimacs-time=`, `midimacs-time%`, etc.

![](https://github.com/andreasjansson/midimacs/blob/master/github-assets/run.gif)


~~~~ _TO BE CONTINUED_ ~~~~
