open! Core

  include String_id.Make
    (struct
      let module_name = "Field_id"
    end)
    ()