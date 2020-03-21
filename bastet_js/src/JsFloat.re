open Interface;

/**
 Promives a Show instance using [Js.Float.toString]. You might not need to use this version
 depending on your use cases.
 See {{:https://github.com/BuckleScript/bucklescript/issues/3412}bucklescript#3412}
 */
module Show: SHOW with type t = float = {
  type t = float;
  let show = Js.Float.toString;
};
