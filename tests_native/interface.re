module QueryWithFragments = [%graphql
  {|
   query {
    users {
      ... on AdminUser {
        id
        name
      }
      ... on AnonymousUser {
        id
        anonymousId
      }
    }
  }
|}
];

type user = [
  | `UnspecifiedFragment(string)
  | `AdminUser(
      {
        .
        id: string,
        name: string,
      },
    )
  | `AnonymousUser(
      {
        .
        id: string,
        anonymousId: int,
      },
    )
];

type only_user = {. id: string};

module QueryWithoutFragments = [%graphql
  {|
   query {
    users {
      id
    }
  }
|}
];

let json = {|{
             "users": [
             { "__typename": "AdminUser", "id": "1", "name": "bob" },
             { "__typename": "AnonymousUser", "id": "2", "anonymousId": 1},
             { "__typename": "OtherUser", "id": "3"}
             ]}|};

let user: module Alcotest.TESTABLE with type t = user =
  (module
   {
     type t = user;

     let pp = formatter =>
       fun
       | `UnspecifiedFragment(s) =>
         Format.fprintf(formatter, "`UnspecifiedFragment < @[%s@] >", s)
       | `AdminUser(u) =>
         Format.fprintf(
           formatter,
           "`AdminUser < id = @[%s@]; name = @[%s@] >",
           u#id,
           u#name,
         )
       | `AnonymousUser(u) =>
         Format.fprintf(
           formatter,
           "`AnonymousUser < id = @[%s@]; anonymousId = @[%i@] >",
           u#id,
           u#anonymousId,
         );

     let equal = (a: user, b: user) =>
       switch (a, b) {
       | (`UnspecifiedFragment(u1), `UnspecifiedFragment(u2)) => u1 == u2
       | (`AdminUser(u1), `AdminUser(u2)) =>
         u1#id == u2#id && u1#name == u2#name
       | (`AnonymousUser(u1), `AnonymousUser(u2)) =>
         u1#id == u2#id && u1#anonymousId == u2#anonymousId
       | _ => false
       };
   });

let only_user: module Alcotest.TESTABLE with type t = only_user =
  (module
   {
     type t = only_user;

     let pp = formatter =>
       fun
       | u => Format.fprintf(formatter, "`User < id = @[%s@] >", u#id);

     let equal = (a: only_user, b: only_user) =>
       switch (a, b) {
       | (u1, u2) => u1#id == u2#id
       };
   });

let decode_with_fragments = () =>
  Alcotest.(check(array(user)))(
    "query result equality",
    QueryWithFragments.parse(Yojson.Basic.from_string(json))#users,
    [|
      `AdminUser({as _; pub id = "1"; pub name = "bob"}),
      `AnonymousUser({as _; pub id = "2"; pub anonymousId = 1}),
    |],
  );

let decode_without_fragments = () =>
  Alcotest.(check(array(only_user)))(
    "query result equality",
    QueryWithoutFragments.parse(Yojson.Basic.from_string(json))#users,
    [|{as _; pub id = "1"}, {as _; pub id = "2"}, {as _; pub id = "3"}|],
  );

let tests = [
  ("Decodes the interface with fragments", `Quick, decode_with_fragments),
  (
    "Decodes the interface without fragments",
    `Quick,
    decode_without_fragments,
  ),
];
