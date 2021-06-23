open! Prelude

let remove_properties =
  List.filter (function
    | Type.Property_Drawer _ -> false
    | _ -> true)
