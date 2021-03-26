let await = Lwt.return;
let (let.await) = Lwt.bind;
let (and.await) = Lwt.both;
let (let+await) = (v, f) => Lwt.map(f, v);

let some = Option.some;
let (let.some) = Option.bind;
let (let+some) = (v, f) => Option.map(f, v);

let none = Option.none;
let (let.none) = (v, f) =>
  switch (v) {
  | Some(v) => Some(v)
  | None => f()
  };
let (let+none) = (v, f) =>
  switch (v) {
  | Some(v) => Some(v)
  | None => f()
  };

let (let.apply) = (v, f) => v(f());

let ok = Result.ok;
let (let.ok) = Result.bind;
