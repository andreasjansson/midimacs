<img width="288" height="223" align="right" src="https://github.com/andreasjansson/midimacs/blob/master/github-assets/logo.png" />

MIDIMACS
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

If a line starts with `>00 `, where `00` can be any two-digit number, it means that line will be a midi track, all other lines are comments. A bit like literal programming!

![](https://github.com/andreasjansson/midimacs/blob/master/github-assets/line-basics.gif)

A midi track line has a sequence of codes. A code is represented by an ascii character. It can be any character except period and space. Period means continue with the previous code (without initialization, we'll get to what that means in a second). Space means "no code".

In the gif above, channel `00` has the code `a` repeated four beats, then re-initialized, then repeated another four beats. In channel `01`, `b` is repeated three times, `c` times, and `d` three times. Everything afterwards is silent.

(NB in midimacs channels are 0-indexed, but most MIDI devices are 1-indexed. So you have to do that arithmetic in your head.)

A code is actually a tiny little algorithm. If you hit RETURN while point is on a code, you'll be taken to that code's buffer, where you'll write the algorithm.

![](https://github.com/andreasjansson/midimacs/blob/master/github-assets/open-code.gif)

Most codes have two functions (well, macros actually, but think of them as functions): `midimacs-init` and `midimacs-run`.

`midimacs-init` is executed the first tick of every beat where the code is initialized, i.e. when the code is a character and not a period. It takes parameters
* _channel_ - the MIDI channel
* _song-time_ - the current position in the song, as a `midimacs-time` struct (see below)
* _length_ - the length of code sequence until it either re-initializes or stops
* _state_ - the current state of the channel (more below)

It returns the new track state, I'll explain that in a bit.

Below is an example of the parameter passed in.

Here are a couple important commands: To evaluate the code buffer, hit `C-c C-c`. If you don't do that, nothing will change no matter how much you type. To start playing your piece, hit `M-SPC`. Same key to stop.

![](https://github.com/andreasjansson/midimacs/blob/master/github-assets/init-params.gif)

`midimacs-run` is executed every tick as long as there's a code in the sequencer. There are 24 ticks per beat.

It takes the same parameters as `midimacs-init`, except _length_ is replaced by _rel-time_ which is the time since the last initialization.

To illustrate `midimacs-run`, let's talk a bit about `midimacs-time` first. A `midimacs-time` struct has two fields, `beat` and `tick`. A number of arithmetic functions are defined for `midimacs-time`, like `midimacs-time+`, `midimacs-time-max`, `midimacs-time=`, `midimacs-time%`, etc.

![](https://github.com/andreasjansson/midimacs/blob/master/github-assets/run.gif)

The state is per track. It's persisted in memory between ticks and is one of the key features for making algorithmic music in midimacs. In the next example we'll use it to print fibonacci numbers.

In the previous gif we printed the current song time twice per beat. There's a macro for that, `midimacs-every`. It takes two arguments, a time symbol and a body to be executed. Time symbols are consise and human readable. For example, `1/2` means half a beat, `2` means two beats, `2+1/2` means two and a half beats, etc.

![](https://github.com/andreasjansson/midimacs/blob/master/github-assets/state.gif)

Now we're ready to make some actual sound. To do that you need to configure your midi output. First make sure you've installed amidicat. Type `M-! amidicat --list` to list the available outputs. `M-x customize-variable midimacs-midi-output-port` and enter your MIDI output port. Once you've customized it you need to reinitialize amidicat from with `midimacs-amidicat-init` (`C-c m A` from the sequencer).

Since gifs are silent I've set `midimacs-midi-output-port` to `DEBUG` which will just print the midi messages in the minibuffer.

To play a note we'll use `midimacs-play-note`. It takes up to 5 arguments:
* _channel_ - midi channel
* _pitch_ - either an int 0 < x < 128 or a pitch symbol like `'C4`, `'Eb9`, `'Ds-1`, etc.
* _duration_ - either a midimacs-time struct or a time symbol like `'1+1/8`, `4`, `1/3`, etc.
* _velocity_ - optional, defaults to the channel velocity
* _off-velocity_ - optional, defaults to 0

![](https://github.com/andreasjansson/midimacs/blob/master/github-assets/play-note.gif)

Quite often you want to create notes non-algorithmically, at least I do. So I made a macro for it: `midimacs-score`.

The body of `midimacs-score` is a sequence of tuples. They can be triples of `(START_TIME PITCH DURATION)` or pairs of `(PITCH DURATION)`. If they're pairs, the start time is just the cumulative sum of previous durations, i.e. the notes are played one after another without pause. To insert a pause, use a hyphen `-` as the pitch. Start time and durations are time symbols as before, and pitch is a pitch symbol.

You can use `C-c m s` (`midimacs-code-show-times`) and `C-c m h` (`midimacs-code-hide-times`) to switch between triples and pairs. I do that in the example here:

![](https://github.com/andreasjansson/midimacs/blob/master/github-assets/score.gif)

You can set loop points in the sequencer with `C-c C-[` and `C-c C-]`. To move the play head to point: `C-<return>`. To start playing at a specific point `C-M-SPC`.

![](https://github.com/andreasjansson/midimacs/blob/master/github-assets/seq-misc.gif)

Midimacs also lets you record MIDI with a real MIDI keyboard. First you need to customize the input port with `M-x customize-variable midimacs-midi-input-port` and run `midimacs-amidicat-init`.

Then, in the sequencer, place the point over the code you want to record and hit `C-c m r`. The buffer for the code will open if it's not already open and recording will start. Any notes you play on the keyboard will be added to the `midimacs-score` macro in the code.

If you're in a loop the score will get overwritten each time it loops around, so you have to be a bit careful to stop at the right moment.

![](https://github.com/andreasjansson/midimacs/blob/master/github-assets/record-midi.gif)

If you don't have a MIDI keyboard around, you can record with your computer keyboard as well. The key _z_ is C2, _s_ is C#2, _x_ is D2, _,_ is C3, _l_ is C#3, _q_ is also C3, _2_ is also C#3, you get the idea. Start recording with `C-c m k`.

When you finished recording you might want to quantize the notes. Quantize the note start times with `C-c m q` and the durations with `C-c m d`.

![](https://github.com/andreasjansson/midimacs/blob/master/github-assets/quantize.gif)

You can save your project with `C-x C-s` as usual, except it will serialize the sequencer and all the codes into a single file. When you're in a midimacs buffer you can open other midimacs projects with `C-x C-f`.

That's pretty much it for the basics! There are some examples in the _examples/_ folder.

If you happen to actually know elisp - I don't really - and wanna help me make the code sane, that'd be great too!

Motivation
----------

I've dabbled in algorithmic composition for a while, and it's cool and everything, but sometimes I just want some actual melodies in there. And some structure!

When I write algorithms to make music it's usually just press go and see where the variables end up. With midimacs I'm able to build a composition where the individual pieces are algorithms. Semi-algorithmic.
