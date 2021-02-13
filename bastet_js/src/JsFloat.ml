open Interface

module Show : SHOW with type t = float = struct
  type t = float

  let show = Js.Float.toString
end
[@@ocaml.doc
  "\n\
  \ Promives a Show instance using [Js.Float.toString]. You might not need to use this version\n\
  \ depending on your use cases.\n\
  \ See {{:https://github.com/BuckleScript/bucklescript/issues/3412}bucklescript#3412}\n\
  \ "]
