# Elm Monocole Generator

Generates lenses for Elm records for use with Elm-Monocle

https://github.com/arturopala/elm-monocle

## Usage

```
runghc Main.hs --module-name "Module.Name" "{ recordField : RecordType }" > ./src/Module/Name.elm
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
