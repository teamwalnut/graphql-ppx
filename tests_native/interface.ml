module QueryWithFragments =
[%graphql
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
|}]

module QueryWithoutFragments =
[%graphql
{|
   query {
    users {
      id
    }
  }
|}]

type user = QueryWithFragments.t_users_User
type only_user = QueryWithoutFragments.t_users

let json =
  {|{
             "users": [
             { "__typename": "AdminUser", "id": "1", "name": "bob" },
             { "__typename": "AnonymousUser", "id": "2", "anonymousId": 1},
             { "__typename": "OtherUser", "id": "3"}
             ]}|}

let user =
  (module struct
    type t = user

    let pp formatter = function
      | `UnspecifiedFragment s ->
        Format.fprintf formatter
          ("`UnspecifiedFragment < @[%s@] >"
          [@reason.raw_literal "`UnspecifiedFragment < @[%s@] >"]) s
      | `AdminUser (u : QueryWithFragments.t_users_User_AdminUser) ->
        Format.fprintf formatter
          ("`AdminUser < id = @[%s@]; name = @[%s@] >"
          [@reason.raw_literal "`AdminUser < id = @[%s@]; name = @[%s@] >"])
          u.id u.name
      | `AnonymousUser (u : QueryWithFragments.t_users_User_AnonymousUser) ->
        Format.fprintf formatter
          ("`AnonymousUser < id = @[%s@]; anonymousId = @[%i@] >"
          [@reason.raw_literal
            "`AnonymousUser < id = @[%s@]; anonymousId = @[%i@] >"]) u.id
          u.anonymousId

    let equal (a : user) (b : user) =
      match (a, b) with
      | `UnspecifiedFragment u1, `UnspecifiedFragment u2 -> u1 = u2
      | `AdminUser u1, `AdminUser u2 -> u1.id = u2.id && u1.name = u2.name
      | `AnonymousUser u1, `AnonymousUser u2 ->
        u1.id = u2.id && u1.anonymousId = u2.anonymousId
      | _ -> false
  end : Alcotest.TESTABLE
    with type t = user)

let only_user =
  (module struct
    type t = only_user

    let pp formatter (u : QueryWithoutFragments.t_users) =
      Format.fprintf formatter
        ("`User < id = @[%s@] >" [@reason.raw_literal "`User < id = @[%s@] >"])
        u.id

    let equal (a : only_user) (b : only_user) =
      match (a, b) with u1, u2 -> u1.id = u2.id
  end : Alcotest.TESTABLE
    with type t = only_user)

let decode_with_fragments () =
  (let open Alcotest in
  check (array user))
    ("query result equality" [@reason.raw_literal "query result equality"])
    (Yojson.Basic.from_string json
    |> QueryWithFragments.unsafe_fromJson |> QueryWithFragments.parse)
      .users
    ([|
       `AdminUser
         {
           id = ("1" [@reason.raw_literal "1"]);
           name = ("bob" [@reason.raw_literal "bob"]);
         };
       `AnonymousUser { id = ("2" [@reason.raw_literal "2"]); anonymousId = 1 };
       `UnspecifiedFragment ("OtherUser" [@reason.raw_literal "OtherUser"]);
     |]
      : QueryWithFragments.t_users_User array)

let decode_without_fragments () =
  (let open Alcotest in
  check (array only_user))
    ("query result equality" [@reason.raw_literal "query result equality"])
    (Yojson.Basic.from_string json
    |> QueryWithoutFragments.unsafe_fromJson |> QueryWithoutFragments.parse)
      .users
    [|
      { id = ("1" [@reason.raw_literal "1"]) };
      { id = ("2" [@reason.raw_literal "2"]) };
      { id = ("3" [@reason.raw_literal "3"]) };
    |]

let tests =
  [
    ( ("Decodes the interface with fragments"
      [@reason.raw_literal "Decodes the interface with fragments"]),
      `Quick,
      decode_with_fragments );
    ( ("Decodes the interface without fragments"
      [@reason.raw_literal "Decodes the interface without fragments"]),
      `Quick,
      decode_without_fragments );
  ]
