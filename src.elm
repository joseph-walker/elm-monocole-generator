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

