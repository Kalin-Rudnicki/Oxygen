package oxygen.sql.error

import oxygen.predef.core.*

enum PSQLCode(final val code: String) {

  // Class 00 — Successful Completion
  case successful_completion extends PSQLCode("00000")

  // Class 01 — Warning
  case warning extends PSQLCode("01000")
  case dynamic_result_sets_returned extends PSQLCode("0100C")
  case implicit_zero_bit_padding extends PSQLCode("01008")
  case null_value_eliminated_in_set_function extends PSQLCode("01003")
  case privilege_not_granted extends PSQLCode("01007")
  case privilege_not_revoked extends PSQLCode("01006")
  case warn_string_data_right_truncation extends PSQLCode("01004")
  case deprecated_feature extends PSQLCode("01P01")

  // Class 02 — No Data (this is also a warning class per the SQL standard)
  case no_data extends PSQLCode("02000")
  case no_additional_dynamic_result_sets_returned extends PSQLCode("02001")

  // Class 03 — SQL Statement Not Yet Complete
  case sql_statement_not_yet_complete extends PSQLCode("03000")

  // Class 08 — Connection Exception
  case connection_exception extends PSQLCode("08000")
  case connection_does_not_exist extends PSQLCode("08003")
  case connection_failure extends PSQLCode("08006")
  case sqlclient_unable_to_establish_sqlconnection extends PSQLCode("08001")
  case sqlserver_rejected_establishment_of_sqlconnection extends PSQLCode("08004")
  case transaction_resolution_unknown extends PSQLCode("08007")
  case protocol_violation extends PSQLCode("08P01")

  // Class 09 — Triggered Action Exception
  case triggered_action_exception extends PSQLCode("09000")

  // Class 0A — Feature Not Supported
  case feature_not_supported extends PSQLCode("0A000")

  // Class 0B — Invalid Transaction Initiation
  case invalid_transaction_initiation extends PSQLCode("0B000")

  // Class 0F — Locator Exception
  case locator_exception extends PSQLCode("0F000")
  case invalid_locator_specification extends PSQLCode("0F001")

  // Class 0L — Invalid Grantor
  case invalid_grantor extends PSQLCode("0L000")
  case invalid_grant_operation extends PSQLCode("0LP01")

  // Class 0P — Invalid Role Specification
  case invalid_role_specification extends PSQLCode("0P000")

  // Class 0Z — Diagnostics Exception
  case diagnostics_exception extends PSQLCode("0Z000")
  case stacked_diagnostics_accessed_without_active_handler extends PSQLCode("0Z002")

  // Class 20 — Case Not Found
  case case_not_found extends PSQLCode("20000")

  // Class 21 — Cardinality Violation
  case cardinality_violation extends PSQLCode("21000")

  // Class 22 — Data Exception
  case data_exception extends PSQLCode("22000")
  case array_subscript_error extends PSQLCode("2202E")
  case character_not_in_repertoire extends PSQLCode("22021")
  case datetime_field_overflow extends PSQLCode("22008")
  case division_by_zero extends PSQLCode("22012")
  case error_in_assignment extends PSQLCode("22005")
  case escape_character_conflict extends PSQLCode("2200B")
  case indicator_overflow extends PSQLCode("22022")
  case interval_field_overflow extends PSQLCode("22015")
  case invalid_argument_for_logarithm extends PSQLCode("2201E")
  case invalid_argument_for_ntile_function extends PSQLCode("22014")
  case invalid_argument_for_nth_value_function extends PSQLCode("22016")
  case invalid_argument_for_power_function extends PSQLCode("2201F")
  case invalid_argument_for_width_bucket_function extends PSQLCode("2201G")
  case invalid_character_value_for_cast extends PSQLCode("22018")
  case invalid_datetime_format extends PSQLCode("22007")
  case invalid_escape_character extends PSQLCode("22019")
  case invalid_escape_octet extends PSQLCode("2200D")
  case invalid_escape_sequence extends PSQLCode("22025")
  case nonstandard_use_of_escape_character extends PSQLCode("22P06")
  case invalid_indicator_parameter_value extends PSQLCode("22010")
  case invalid_parameter_value extends PSQLCode("22023")
  case invalid_preceding_or_following_size extends PSQLCode("22013")
  case invalid_regular_expression extends PSQLCode("2201B")
  case invalid_row_count_in_limit_clause extends PSQLCode("2201W")
  case invalid_row_count_in_result_offset_clause extends PSQLCode("2201X")
  case invalid_tablesample_argument extends PSQLCode("2202H")
  case invalid_tablesample_repeat extends PSQLCode("2202G")
  case invalid_time_zone_displacement_value extends PSQLCode("22009")
  case invalid_use_of_escape_character extends PSQLCode("2200C")
  case most_specific_type_mismatch extends PSQLCode("2200G")
  case null_value_not_allowed extends PSQLCode("22004")
  case null_value_no_indicator_parameter extends PSQLCode("22002")
  case numeric_value_out_of_range extends PSQLCode("22003")
  case sequence_generator_limit_exceeded extends PSQLCode("2200H")
  case string_data_length_mismatch extends PSQLCode("22026")
  case string_data_right_truncation extends PSQLCode("22001")
  case substring_error extends PSQLCode("22011")
  case trim_error extends PSQLCode("22027")
  case unterminated_c_string extends PSQLCode("22024")
  case zero_length_character_string extends PSQLCode("2200F")
  case floating_point_exception extends PSQLCode("22P01")
  case invalid_text_representation extends PSQLCode("22P02")
  case invalid_binary_representation extends PSQLCode("22P03")
  case bad_copy_file_format extends PSQLCode("22P04")
  case untranslatable_character extends PSQLCode("22P05")
  case not_an_xml_document extends PSQLCode("2200L")
  case invalid_xml_document extends PSQLCode("2200M")
  case invalid_xml_content extends PSQLCode("2200N")
  case invalid_xml_comment extends PSQLCode("2200S")
  case invalid_xml_processing_instruction extends PSQLCode("2200T")
  case duplicate_json_object_key_value extends PSQLCode("22030")
  case invalid_argument_for_sql_json_datetime_function extends PSQLCode("22031")
  case invalid_json_text extends PSQLCode("22032")
  case invalid_sql_json_subscript extends PSQLCode("22033")
  case more_than_one_sql_json_item extends PSQLCode("22034")
  case no_sql_json_item extends PSQLCode("22035")
  case non_numeric_sql_json_item extends PSQLCode("22036")
  case non_unique_keys_in_a_json_object extends PSQLCode("22037")
  case singleton_sql_json_item_required extends PSQLCode("22038")
  case sql_json_array_not_found extends PSQLCode("22039")
  case sql_json_member_not_found extends PSQLCode("2203A")
  case sql_json_number_not_found extends PSQLCode("2203B")
  case sql_json_object_not_found extends PSQLCode("2203C")
  case too_many_json_array_elements extends PSQLCode("2203D")
  case too_many_json_object_members extends PSQLCode("2203E")
  case sql_json_scalar_required extends PSQLCode("2203F")
  case sql_json_item_cannot_be_cast_to_target_type extends PSQLCode("2203G")

  // Class 23 — Integrity Constraint Violation
  case integrity_constraint_violation extends PSQLCode("23000")
  case restrict_violation extends PSQLCode("23001")
  case not_null_violation extends PSQLCode("23502")
  case foreign_key_violation extends PSQLCode("23503")
  case unique_violation extends PSQLCode("23505")
  case check_violation extends PSQLCode("23514")
  case exclusion_violation extends PSQLCode("23P01")

  // Class 24 — Invalid Cursor State
  case invalid_cursor_state extends PSQLCode("24000")

  // Class 25 — Invalid Transaction State
  case invalid_transaction_state extends PSQLCode("25000")
  case active_sql_transaction extends PSQLCode("25001")
  case branch_transaction_already_active extends PSQLCode("25002")
  case held_cursor_requires_same_isolation_level extends PSQLCode("25008")
  case inappropriate_access_mode_for_branch_transaction extends PSQLCode("25003")
  case inappropriate_isolation_level_for_branch_transaction extends PSQLCode("25004")
  case no_active_sql_transaction_for_branch_transaction extends PSQLCode("25005")
  case read_only_sql_transaction extends PSQLCode("25006")
  case schema_and_data_statement_mixing_not_supported extends PSQLCode("25007")
  case no_active_sql_transaction extends PSQLCode("25P01")
  case in_failed_sql_transaction extends PSQLCode("25P02")
  case idle_in_transaction_session_timeout extends PSQLCode("25P03")
  case transaction_timeout extends PSQLCode("25P04")

  // Class 26 — Invalid SQL Statement Name
  case invalid_sql_statement_name extends PSQLCode("26000")

  // Class 27 — Triggered Data Change Violation
  case triggered_data_change_violation extends PSQLCode("27000")

  // Class 28 — Invalid Authorization Specification
  case invalid_authorization_specification extends PSQLCode("28000")
  case invalid_password extends PSQLCode("28P01")

  // Class 2B — Dependent Privilege Descriptors Still Exist
  case dependent_privilege_descriptors_still_exist extends PSQLCode("2B000")
  case dependent_objects_still_exist extends PSQLCode("2BP01")

  // Class 2D — Invalid Transaction Termination
  case invalid_transaction_termination extends PSQLCode("2D000")

  // Class 2F — SQL Routine Exception
  case sql_routine_exception extends PSQLCode("2F000")
  case function_executed_no_return_statement extends PSQLCode("2F005")
  case sql_modifying_sql_data_not_permitted extends PSQLCode("2F002")
  case sql_prohibited_sql_statement_attempted extends PSQLCode("2F003")
  case sql_reading_sql_data_not_permitted extends PSQLCode("2F004")

  // Class 34 — Invalid Cursor Name
  case invalid_cursor_name extends PSQLCode("34000")

  // Class 38 — External Routine Exception
  case external_routine_exception extends PSQLCode("38000")
  case containing_sql_not_permitted extends PSQLCode("38001")
  case external_modifying_sql_data_not_permitted extends PSQLCode("38002")
  case external_prohibited_sql_statement_attempted extends PSQLCode("38003")
  case external_reading_sql_data_not_permitted extends PSQLCode("38004")

  // Class 39 — External Routine Invocation Exception
  case external_routine_invocation_exception extends PSQLCode("39000")
  case invalid_sqlstate_returned extends PSQLCode("39001")
  case external_null_value_not_allowed extends PSQLCode("39004")
  case trigger_protocol_violated extends PSQLCode("39P01")
  case srf_protocol_violated extends PSQLCode("39P02")
  case event_trigger_protocol_violated extends PSQLCode("39P03")

  // Class 3B — Savepoint Exception
  case savepoint_exception extends PSQLCode("3B000")
  case invalid_savepoint_specification extends PSQLCode("3B001")

  // Class 3D — Invalid Catalog Name
  case invalid_catalog_name extends PSQLCode("3D000")

  // Class 3F — Invalid Schema Name
  case invalid_schema_name extends PSQLCode("3F000")

  // Class 40 — Transaction Rollback
  case transaction_rollback extends PSQLCode("40000")
  case transaction_integrity_constraint_violation extends PSQLCode("40002")
  case serialization_failure extends PSQLCode("40001")
  case statement_completion_unknown extends PSQLCode("40003")
  case deadlock_detected extends PSQLCode("40P01")

  // Class 42 — Syntax Error or Access Rule Violation
  case syntax_error_or_access_rule_violation extends PSQLCode("42000")
  case syntax_error extends PSQLCode("42601")
  case insufficient_privilege extends PSQLCode("42501")
  case cannot_coerce extends PSQLCode("42846")
  case grouping_error extends PSQLCode("42803")
  case windowing_error extends PSQLCode("42P20")
  case invalid_recursion extends PSQLCode("42P19")
  case invalid_foreign_key extends PSQLCode("42830")
  case invalid_name extends PSQLCode("42602")
  case name_too_long extends PSQLCode("42622")
  case reserved_name extends PSQLCode("42939")
  case datatype_mismatch extends PSQLCode("42804")
  case indeterminate_datatype extends PSQLCode("42P18")
  case collation_mismatch extends PSQLCode("42P21")
  case indeterminate_collation extends PSQLCode("42P22")
  case wrong_object_type extends PSQLCode("42809")
  case generated_always extends PSQLCode("428C9")
  case undefined_column extends PSQLCode("42703")
  case undefined_function extends PSQLCode("42883")
  case undefined_table extends PSQLCode("42P01")
  case undefined_parameter extends PSQLCode("42P02")
  case undefined_object extends PSQLCode("42704")
  case duplicate_column extends PSQLCode("42701")
  case duplicate_cursor extends PSQLCode("42P03")
  case duplicate_database extends PSQLCode("42P04")
  case duplicate_function extends PSQLCode("42723")
  case duplicate_prepared_statement extends PSQLCode("42P05")
  case duplicate_schema extends PSQLCode("42P06")
  case duplicate_table extends PSQLCode("42P07")
  case duplicate_alias extends PSQLCode("42712")
  case duplicate_object extends PSQLCode("42710")
  case ambiguous_column extends PSQLCode("42702")
  case ambiguous_function extends PSQLCode("42725")
  case ambiguous_parameter extends PSQLCode("42P08")
  case ambiguous_alias extends PSQLCode("42P09")
  case invalid_column_reference extends PSQLCode("42P10")
  case invalid_column_definition extends PSQLCode("42611")
  case invalid_cursor_definition extends PSQLCode("42P11")
  case invalid_database_definition extends PSQLCode("42P12")
  case invalid_function_definition extends PSQLCode("42P13")
  case invalid_prepared_statement_definition extends PSQLCode("42P14")
  case invalid_schema_definition extends PSQLCode("42P15")
  case invalid_table_definition extends PSQLCode("42P16")
  case invalid_object_definition extends PSQLCode("42P17")

  // Class 44 — WITH CHECK OPTION Violation
  case with_check_option_violation extends PSQLCode("44000")

  // Class 53 — Insufficient Resources
  case insufficient_resources extends PSQLCode("53000")
  case disk_full extends PSQLCode("53100")
  case out_of_memory extends PSQLCode("53200")
  case too_many_connections extends PSQLCode("53300")
  case configuration_limit_exceeded extends PSQLCode("53400")

  // Class 54 — Program Limit Exceeded
  case program_limit_exceeded extends PSQLCode("54000")
  case statement_too_complex extends PSQLCode("54001")
  case too_many_columns extends PSQLCode("54011")
  case too_many_arguments extends PSQLCode("54023")

  // Class 55 — Object Not In Prerequisite State
  case object_not_in_prerequisite_state extends PSQLCode("55000")
  case object_in_use extends PSQLCode("55006")
  case cant_change_runtime_param extends PSQLCode("55P02")
  case lock_not_available extends PSQLCode("55P03")
  case unsafe_new_enum_value_usage extends PSQLCode("55P04")

  // Class 57 — Operator Intervention
  case operator_intervention extends PSQLCode("57000")
  case query_canceled extends PSQLCode("57014")
  case admin_shutdown extends PSQLCode("57P01")
  case crash_shutdown extends PSQLCode("57P02")
  case cannot_connect_now extends PSQLCode("57P03")
  case database_dropped extends PSQLCode("57P04")
  case idle_session_timeout extends PSQLCode("57P05")

  // Class 58 — System Error (errors external to PostgreSQL itself)
  case system_error extends PSQLCode("58000")
  case io_error extends PSQLCode("58030")
  case undefined_file extends PSQLCode("58P01")
  case duplicate_file extends PSQLCode("58P02")

  // Class F0 — Configuration File Error
  case config_file_error extends PSQLCode("F0000")
  case lock_file_exists extends PSQLCode("F0001")

  // Class HV — Foreign Data Wrapper Error (SQL/MED)
  case fdw_error extends PSQLCode("HV000")
  case fdw_column_name_not_found extends PSQLCode("HV005")
  case fdw_dynamic_parameter_value_needed extends PSQLCode("HV002")
  case fdw_function_sequence_error extends PSQLCode("HV010")
  case fdw_inconsistent_descriptor_information extends PSQLCode("HV021")
  case fdw_invalid_attribute_value extends PSQLCode("HV024")
  case fdw_invalid_column_name extends PSQLCode("HV007")
  case fdw_invalid_column_number extends PSQLCode("HV008")
  case fdw_invalid_data_type extends PSQLCode("HV004")
  case fdw_invalid_data_type_descriptors extends PSQLCode("HV006")
  case fdw_invalid_descriptor_field_identifier extends PSQLCode("HV091")
  case fdw_invalid_handle extends PSQLCode("HV00B")
  case fdw_invalid_option_index extends PSQLCode("HV00C")
  case fdw_invalid_option_name extends PSQLCode("HV00D")
  case fdw_invalid_string_length_or_buffer_length extends PSQLCode("HV090")
  case fdw_invalid_string_format extends PSQLCode("HV00A")
  case fdw_invalid_use_of_null_pointer extends PSQLCode("HV009")
  case fdw_too_many_handles extends PSQLCode("HV014")
  case fdw_out_of_memory extends PSQLCode("HV001")
  case fdw_no_schemas extends PSQLCode("HV00P")
  case fdw_option_name_not_found extends PSQLCode("HV00J")
  case fdw_reply_handle extends PSQLCode("HV00K")
  case fdw_schema_not_found extends PSQLCode("HV00Q")
  case fdw_table_not_found extends PSQLCode("HV00R")
  case fdw_unable_to_create_execution extends PSQLCode("HV00L")
  case fdw_unable_to_create_reply extends PSQLCode("HV00M")
  case fdw_unable_to_establish_connection extends PSQLCode("HV00N")

  // Class P0 — PL/pgSQL Error
  case plpgsql_error extends PSQLCode("P0000")
  case raise_exception extends PSQLCode("P0001")
  case no_data_found extends PSQLCode("P0002")
  case too_many_rows extends PSQLCode("P0003")
  case assert_failure extends PSQLCode("P0004")

  // Class XX — Internal Error
  case internal_error extends PSQLCode("XX000")
  case data_corrupted extends PSQLCode("XX001")
  case index_corrupted extends PSQLCode("XX002")

}
object PSQLCode {

  val byName: StrictEnum[PSQLCode] = StrictEnum.deriveSingle(_.toString)
  val byCode: StrictEnum[PSQLCode] = StrictEnum.deriveSingle[PSQLCode](_.code)

}
