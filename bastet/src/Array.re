module A =
  ArrayF.Make({
    let length = ArrayLabels.length;
    let make = ArrayLabels.make;
    let append = ArrayLabels.append;
    let map = (f, arr) => ArrayLabels.map(~f, arr);
    let mapi = (f, arr) =>
      ArrayLabels.mapi(~f=(index, e) => f(e, index), arr);
    let fold_left = (f, init, arr) => ArrayLabels.fold_left(~f, ~init, arr);
    let every = (f, arr) => ArrayLabels.for_all(~f, arr);
    let slice = (~start, ~end_, arr) =>
      ArrayLabels.sub(arr, ~pos=start, ~len=end_ - start);
  });

include A;
