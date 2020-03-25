/**
 This module provdes [Js.Array]-sepcific implementations for those who want things to compile into
 clean javascript code. You can still use {!Array} on the JS side if this doesn't matter to you.
 */

module A =
  ArrayF.Make({
    let length = Js.Array.length;
    let make = (n, value) => {
      let arr = [||];
      for (_ in 1 to n) {
        Js.Array.push(value, arr) |> ignore;
      };
      arr;
    };
    let append = Belt.Array.concat;
    let map = Js.Array.map;
    let mapi = Js.Array.mapi;
    let fold_left = Js.Array.reduce;
    let every = Js.Array.every;
    let slice = Js.Array.slice;
  });

include A;
