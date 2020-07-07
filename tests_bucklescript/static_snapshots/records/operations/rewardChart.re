[@ocaml.ppx.context
  {
    tool_name: "migrate_driver",
    include_dirs: [],
    load_path: [],
    open_modules: [],
    for_package: None,
    debug: false,
    use_threads: false,
    use_vmthreads: false,
    recursive_types: false,
    principal: false,
    transparent_modules: false,
    unboxed_types: false,
    unsafe_string: false,
    cookies: [],
  }
];

module Query = {
  [@ocaml.warning "-32"];
  module Raw = {
    type t_goalFeedItem_goal = {
      id: string,
      title: Js.Nullable.t(string),
      instruction: Js.Nullable.t(string),
    };
    type t_goalFeedItem_rewardChartEntries = {
      id: string,
      target: Js.Nullable.t(int),
    };
    type t_goalFeedItem;
    type t = {goalFeedItem: Js.Nullable.t(t_goalFeedItem)};
    type t_variables = {
      id: string,
      pixelRatio: float,
    };
  };
  type t_goalFeedItem_goal = {
    id: string,
    title: option(string),
    instruction: option(string),
  };
  type t_goalFeedItem_rewardChartEntries = {
    id: string,
    target: option(int),
  };
  type t_goalFeedItem = {
    id: string,
    goalResultAvatars_GoalFeedItem: GoalResultAvatars_Fragments.GoalResultAvatars_GoalFeedItem.t_GoalFeedItem,
    rewardChart_GoalFeedItem: RewardChart_Fragments.Fragments.RewardChart_GoalFeedItem.t_GoalFeedItem,
    scheduledAt: Js.Json.t,
    completedAt: Js.Json.t,
    goal: t_goalFeedItem_goal,
    rewardChartEntries: array(t_goalFeedItem_rewardChartEntries),
  };
  type t = {goalFeedItem: option(t_goalFeedItem)};
  /**The GraphQL query string*/
  let query =
    (
      "query GoalOverviewQuery($id: ID!, $pixelRatio: Float!)  {\ngoalFeedItem(id: $id)  {\nid  \n...GoalResultAvatars_Fragments.GoalResultAvatars_GoalFeedItem   \n...RewardChart_Fragments.Fragments.RewardChart_GoalFeedItem   \nscheduledAt  \ncompletedAt  \ngoal  {\nid  \ntitle  \ninstruction  \n}\n\nrewardChartEntries  {\nid  \ntarget  \n}\n\n}\n\n}\n"
      ++ GoalResultAvatars_Fragments.GoalResultAvatars_GoalFeedItem.query
    )
    ++ RewardChart_Fragments.Fragments.RewardChart_GoalFeedItem.query;
  type t_variables = {
    id: string,
    pixelRatio: float,
  };
  /**Parse the JSON GraphQL data to ReasonML data types*/
  let parse = (value: Raw.t): t => (
    {
      goalFeedItem: {
        let value = (value: Raw.t).goalFeedItem;
        switch (Js.toOption(value)) {
        | Some(value) =>
          Some(
            {
              id: {
                let value =
                  Obj.magic(Js.Dict.unsafeGet(Obj.magic(value), "id"));
                value;
              },
              goalResultAvatars_GoalFeedItem: {
                let value: GoalResultAvatars_Fragments.GoalResultAvatars_GoalFeedItem.Raw.t =
                  Obj.magic(value);

                GoalResultAvatars_Fragments.GoalResultAvatars_GoalFeedItem.verifyArgsAndParse(
                  ~pixelRatio=`Float_NonNull,
                  ~fragmentName=`GoalResultAvatars_Fragments.GoalResultAvatars_GoalFeedItem,
                  value,
                );
              },
              rewardChart_GoalFeedItem: {
                let value: RewardChart_Fragments.Fragments.RewardChart_GoalFeedItem.Raw.t =
                  Obj.magic(value);

                RewardChart_Fragments.Fragments.RewardChart_GoalFeedItem.verifyArgsAndParse(
                  ~pixelRatio=`Float_NonNull,
                  ~fragmentName=`RewardChart_Fragments.Fragments.RewardChart_GoalFeedItem,
                  value,
                );
              },
              scheduledAt: {
                let value =
                  Obj.magic(
                    Js.Dict.unsafeGet(Obj.magic(value), "scheduledAt"),
                  );
                value;
              },
              completedAt: {
                let value =
                  Obj.magic(
                    Js.Dict.unsafeGet(Obj.magic(value), "completedAt"),
                  );
                value;
              },
              goal: {
                let value =
                  Obj.magic(Js.Dict.unsafeGet(Obj.magic(value), "goal"));
                (
                  {
                    id: {
                      let value = (value: Raw.t_goalFeedItem_goal).id;
                      value;
                    },
                    title: {
                      let value = (value: Raw.t_goalFeedItem_goal).title;
                      switch (Js.toOption(value)) {
                      | Some(value) => Some(value)
                      | None => None
                      };
                    },
                    instruction: {
                      let value = (value: Raw.t_goalFeedItem_goal).instruction;
                      switch (Js.toOption(value)) {
                      | Some(value) => Some(value)
                      | None => None
                      };
                    },
                  }: t_goalFeedItem_goal
                );
              },
              rewardChartEntries: {
                let value =
                  Obj.magic(
                    Js.Dict.unsafeGet(
                      Obj.magic(value),
                      "rewardChartEntries",
                    ),
                  );
                value
                |> Js.Array.map((value) =>
                     (
                       {
                         id: {
                           let value =
                             (value: Raw.t_goalFeedItem_rewardChartEntries).id;
                           value;
                         },
                         target: {
                           let value =
                             (value: Raw.t_goalFeedItem_rewardChartEntries).
                               target;
                           switch (Js.toOption(value)) {
                           | Some(value) => Some(value)
                           | None => None
                           };
                         },
                       }: t_goalFeedItem_rewardChartEntries
                     )
                   );
              },
            }: t_goalFeedItem,
          )
        | None => None
        };
      },
    }: t
  );
  /**Serialize the ReasonML GraphQL data that was parsed using the parse function back to the original JSON compatible data */
  let serialize = (value: t): Raw.t => (
    {
      let goalFeedItem = {
        let value = (value: t).goalFeedItem;
        switch (value) {
        | Some(value) =>
          Js.Nullable.return(
            Obj.magic(
              Js.Array.reduce(
                GraphQL_PPX.deepMerge,
                Obj.magic(
                  {
                    let rewardChartEntries = {
                      let value = (value: t_goalFeedItem).rewardChartEntries;
                      value
                      |> Js.Array.map((value) =>
                           (
                             {
                               let target = {
                                 let value =
                                   (value: t_goalFeedItem_rewardChartEntries).
                                     target;
                                 switch (value) {
                                 | Some(value) => Js.Nullable.return(value)
                                 | None => Js.Nullable.null
                                 };
                               }
                               and id = {
                                 let value =
                                   (value: t_goalFeedItem_rewardChartEntries).
                                     id;
                                 value;
                               };
                               {id, target};
                             }: Raw.t_goalFeedItem_rewardChartEntries
                           )
                         );
                    }
                    and goal = {
                      let value = (value: t_goalFeedItem).goal;
                      (
                        {
                          let instruction = {
                            let value =
                              (value: t_goalFeedItem_goal).instruction;
                            switch (value) {
                            | Some(value) => Js.Nullable.return(value)
                            | None => Js.Nullable.null
                            };
                          }
                          and title = {
                            let value = (value: t_goalFeedItem_goal).title;
                            switch (value) {
                            | Some(value) => Js.Nullable.return(value)
                            | None => Js.Nullable.null
                            };
                          }
                          and id = {
                            let value = (value: t_goalFeedItem_goal).id;
                            value;
                          };
                          {id, title, instruction};
                        }: Raw.t_goalFeedItem_goal
                      );
                    }
                    and completedAt = {
                      let value = (value: t_goalFeedItem).completedAt;
                      value;
                    }
                    and scheduledAt = {
                      let value = (value: t_goalFeedItem).scheduledAt;
                      value;
                    }
                    and id = {
                      let value = (value: t_goalFeedItem).id;
                      value;
                    };
                    {
                      "id": id,
                      "scheduledAt": scheduledAt,
                      "completedAt": completedAt,
                      "goal": goal,
                      "rewardChartEntries": rewardChartEntries,
                    };
                  },
                ): Js.Json.t,
                [|
                  (
                    Obj.magic(
                      GoalResultAvatars_Fragments.GoalResultAvatars_GoalFeedItem.serialize(
                        (value: t_goalFeedItem).goalResultAvatars_GoalFeedItem,
                      ),
                    ): Js.Json.t
                  ),
                  (
                    Obj.magic(
                      RewardChart_Fragments.Fragments.RewardChart_GoalFeedItem.serialize(
                        (value: t_goalFeedItem).rewardChart_GoalFeedItem,
                      ),
                    ): Js.Json.t
                  ),
                |],
              ),
            ): Raw.t_goalFeedItem,
          )
        | None => Js.Nullable.null
        };
      };
      {goalFeedItem: goalFeedItem};
    }: Raw.t
  );
  let serializeVariables: t_variables => Raw.t_variables =
    inp => {
      id: (a => a)((inp: t_variables).id),
      pixelRatio: (a => a)((inp: t_variables).pixelRatio),
    };
  let makeVariables = (~id, ~pixelRatio, ()) =>
    serializeVariables({id, pixelRatio}: t_variables);
  external unsafe_fromJson: Js.Json.t => Raw.t = "%identity";
  external toJson: Raw.t => Js.Json.t = "%identity";
  external variablesToJson: Raw.t_variables => Js.Json.t = "%identity";
  module Z__INTERNAL = {
    type nonrec _graphql_id_110;
    /**Variable **$id** has the following graphql type:

```
ID!
```*/
    let _graphql_id_110: _graphql_id_110 = Obj.magic(0);
    type nonrec _graphql_id_106;
    /**Argument **id** on field **goalFeedItem** has the following graphql type:

```
ID!
```*/
    let _graphql_id_106: _graphql_id_106 = Obj.magic(0);
    type root = t;
    type nonrec graphql_module;
    /****--- graphql-ppx module ---**

The contents of this module are automatically generated by `graphql-ppx`.
The following is simply an overview of the most important variables and types that you can access from this module.

```
module Query {
  /**
  The GraphQL query string
  */
  let query: string;

  /**
  This is the main type of the result you will get back.
  You can hover above the identifier key (e.g. query or mutation) to see the fully generated type for your module.
  */
  type t;

  /**
  Parse the JSON GraphQL data to ReasonML data types
  */
  let parse: Raw.t => t;

  /**
  Serialize the ReasonML GraphQL data that was parsed using the parse function back to the original JSON compatible data
  */
  let serialize: t => Raw.t;

  /**
  This is the JSON compatible type of the GraphQL data.
  It should not be necessary to access the types inside for normal use cases.
  */
  module Raw: { type t; };
}
```*/
    let graphql_module: graphql_module = Obj.magic(0);
  };
};
