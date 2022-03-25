open Traversal_utils

module AllRulesImpl =
  Multi_visitor.Visitor
    (Rule_no_invalid_default_values.Visitor)
    (Multi_visitor.Visitor
       (Rule_no_unused_variables.Visitor)
       (Multi_visitor.Visitor
          (Rule_no_undefined_variables.Visitor)
          (Multi_visitor.Visitor
             (Rule_no_invalid_input_object_values.Visitor)
             (Multi_visitor.Visitor
                (Rule_all_required_arguments.Visitor)
                (Multi_visitor.Visitor
                   (Rule_known_argument_names.Visitor)
                   (Multi_visitor.Visitor
                      (Rule_deprecated_fields.Visitor)
                      (Multi_visitor.NullVisitor)))))))

module AllRules = Visitor (AllRulesImpl)

let run_validators config document =
  let ctx = make_context config document in
  let _ = AllRules.visit_document ctx document in
  ((match !(ctx.errors) with [] -> None | errs -> Some errs), !(ctx.warnings))
