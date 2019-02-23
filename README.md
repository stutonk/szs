szs
===
Chez Scheme clone of [szsol](https://github.com/usrshare/szsol)

### About
This is a [Termbox](https://github.com/nsf/termbox)-based implementation of
Shenzhen Solitaire from [Shenzhen I/O](http://www.zachtronics.com/shenzhen-io/).
It was originally created as an experiment to test the viability of Chez for 
application development. As such, there's not much in-source documentation.

### Running
The path of least resistance is to pop into a Chez repl, `(load "szs.scm")`,
and `(start-game)`. N.B. the included termbox FFI binding assumes the lib's 
shared object lives at `/usr/lib/libtermbox.so.1.0.0`. Adjust as necessary.
