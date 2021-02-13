module A = ArrayF.Make (struct
  let length = Js.Array.length

  let make n value =
    let arr = [||] in
    for _ = 1 to n do
      Js.Array.push value arr |> ignore
    done;
    arr

  let append = Belt.Array.concat

  let map = Js.Array.map

  let mapi = Js.Array.mapi

  let fold_left = Js.Array.reduce

  let every = Js.Array.every

  let slice = Js.Array.slice
end)
[@@ocaml.doc
  "\n\
  \ This module provdes [Js.Array]-sepcific implementations for those who want things to compile \
   into\n\
  \ clean javascript code. You can still use {!Array} on the JS side if this doesn't matter to you.\n\
  \ "]

include A
