# Elm Monocole Generator

Generates lenses for Elm records for use with Elm-Monocle

https://github.com/arturopala/elm-monocle

## Usage

__Requirements__
* GHC
* Parsec

1) Build the Project for your Platform
```
ghc -o "./bin/elm-make-lenses" -outputdir "./build" Main.hs
```

2) Link the executable into your path
```
ln -s <Project Path>/bin/elm-make-lenses /usr/local/bin/elm-make-lenses
```

3) Make Lenses
```
elm-make-lenses --module-name "Module.Name" "{ recordField : RecordType }"
```

## Example Output

```
module Module.Name exposing (..)

import Monocle.Optional exposing (Optional)
import Monocle.Lens exposing (Lens)

recordField : Lens { recordField : RecordType } RecordType
recordField =
	let
		set value model =
			{ model | recordField = value }
	in
		Lens .recordField set
```
