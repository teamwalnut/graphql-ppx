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

module QueryWithoutFragments = [%graphql
  {|
   query {
    users {
      id
    }
  }
|}
];

type user = QueryWithFragments.t_users_User;
type only_user = QueryWithoutFragments.t_users;

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
       | `AdminUser(u: QueryWithFragments.t_users_User_AdminUser) =>
         Format.fprintf(
           formatter,
           "`AdminUser < id = @[%s@]; name = @[%s@] >",
           u.id,
           u.name,
         )
       | `AnonymousUser(u: QueryWithFragments.t_users_User_AnonymousUser) =>
         Format.fprintf(
           formatter,
           "`AnonymousUser < id = @[%s@]; anonymousId = @[%i@] >",
           u.id,
           u.anonymousId,
         );

     let equal = (a: user, b: user) =>
       switch (a, b) {
       | (`UnspecifiedFragment(u1), `UnspecifiedFragment(u2)) => u1 == u2
       | (`AdminUser(u1), `AdminUser(u2)) =>
         u1.id == u2.id && u1.name == u2.name
       | (`AnonymousUser(u1), `AnonymousUser(u2)) =>
         u1.id == u2.id && u1.anonymousId == u2.anonymousId
       | _ => false
       };
   });

let only_user: module Alcotest.TESTABLE with type t = only_user =
  (module
   {
     type t = only_user;

     let pp = (formatter, u: QueryWithoutFragments.t_users) =>
       Format.fprintf(formatter, "`User < id = @[%s@] >", u.id);

     let equal = (a: only_user, b: only_user) =>
       switch (a, b) {
       | (u1, u2) => u1.id == u2.id
       };
   });

let decode_with_fragments = () =>
  Alcotest.(check(array(user)))(
    "query result equality",
    (
      Yojson.Basic.from_string(json)
      |> QueryWithFragments.unsafe_fromJson
      |> QueryWithFragments.parse
    ).
      users,
    [|
      `AdminUser({id: "1", name: "bob"}),
      `AnonymousUser({id: "2", anonymousId: 1}),
      `UnspecifiedFragment("otherVariant"),
    |],
  );

let decode_without_fragments = () =>
  Alcotest.(check(array(only_user)))(
    "query result equality",
    (
      Yojson.Basic.from_string(json)
      |> QueryWithoutFragments.unsafe_fromJson
      |> QueryWithoutFragments.parse
    ).
      users,
    [|{id: "1"}, {id: "2"}, {id: "3"}|],
  );

let tests = [
  ("Decodes the interface with fragments", `Quick, decode_with_fragments),
  (
    "Decodes the interface without fragments",
    `Quick,
    decode_without_fragments,
  ),
];
