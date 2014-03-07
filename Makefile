deps:
	git clone git@github.com:purescript/purescript-jquery.git
	git clone git@github.com:purescript/purescript-reactive.git
	git clone git@github.com:purescript/purescript-json.git

all: lib test

lib:
	mkdir -p js/Control/Reactive/
	psc purescript-json/src/Data.JSON.purs \
            purescript-reactive/src/Control/Reactive.purs.hs \
            purescript-jquery/src/Control/Monad/JQuery.purs.hs \
	    src/Control/Reactive/JQuery.purs.hs \
	  -o js/Control/Reactive/JQuery.js \
	  -e js/Control/Reactive/JQuery.e.purs.hs \
	  --magic-do --module Control.Reactive.JQuery --module Data.Either --module Control.Reactive

test:
	mkdir -p js/
	psc purescript-json/src/Data.JSON.purs \
            purescript-reactive/src/Control/Reactive.purs.hs \
            purescript-jquery/src/Control/Monad/JQuery.purs.hs \
	    src/Control/Reactive/JQuery.purs.hs \
	    examples/test.purs.hs \
	  -o js/test.js \
	  --main --magic-do --module Main --module Data.Either --module Control.Reactive

