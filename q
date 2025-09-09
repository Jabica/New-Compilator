git push origin mainRevert "Merge pull request #3 from Jabica/proR2-01"

This reverts commit 25ed1c937c9f644f24306cf980f7bb0597502fbe, reversing
changes made to 95bce6ca317b366733b9f20d63119a3509158c91.

# Please enter the commit message for your changes. Lines starting
# with '#' will be ignored, and an empty message aborts the commit.
#
# On branch main
# Your branch is up to date with 'origin/main'.
#
# Changes to be committed:
#	modified:   build/.ninja_deps
#	modified:   build/.ninja_log
#	modified:   build/CMakeFiles/4.1.1/CMakeDetermineCompilerABI_C.bin
#	modified:   build/CMakeFiles/4.1.1/CMakeDetermineCompilerABI_CXX.bin
#	modified:   build/CMakeFiles/CMakeConfigureLog.yaml
#	modified:   build/CMakeFiles/mycc.dir/src/cli.cpp.o
#	modified:   build/CMakeFiles/mycc.dir/src/codegen.cpp.o
#	modified:   build/CMakeFiles/mycc.dir/src/lexer.cpp.o
#	modified:   build/CMakeFiles/mycc.dir/src/parser.cpp.o
#	modified:   build/libmycc.a
#	modified:   build/libmycc_runtime.a
#	modified:   build/mycc_cli
#	deleted:    scripts/run_derived_view_tests.sh
#	deleted:    scripts/run_strided_view_tests.sh
#	modified:   scripts/run_tests.sh
#	deleted:    scripts/run_view_ops_tests.sh
#	deleted:    scripts/run_view_smartcopy_tests.sh
#	modified:   src/ast.hpp
#	modified:   src/cli.cpp
#	modified:   src/codegen.cpp
#	modified:   src/codegen.hpp
#	modified:   src/lexer.cpp
#	modified:   src/parser.cpp
#	modified:   src/parser.hpp
#	modified:   src/semantics.hpp
#	modified:   src/token.hpp
#	deleted:    tests/107_copy_len_mismatch.my
#	deleted:    tests/107_do_while_cond_not_bool.my
#	deleted:    tests/108_fill_type_mismatch.my
#	deleted:    tests/111_copy2d_bad_arity.my
#	deleted:    tests/111_copy_shape_mismatch.my
#	deleted:    tests/112_fill2d_bad_arity.my
#	deleted:    tests/112_fill_view_bad_rhs.my
#	deleted:    tests/113_copy2d_nonvar_dst.my
#	deleted:    tests/113_slice_bad_len.my
#	deleted:    tests/114_slice_bad_step.my
#	deleted:    tests/124_switch_duplicate_case.my
#	deleted:    tests/125_switch_non_int_cond.my
#	deleted:    tests/126_switch_non_int_case.my
#	deleted:    tests/127_break_outside_loop_or_switch.my
#	deleted:    tests/128_fallthrough_outside_switch.my
#	deleted:    tests/129_fallthrough_not_last_stmt.my
#	deleted:    tests/130_fallthrough_in_last_case_without_next.my
#	deleted:    tests/131_continue_outside_while.my
#	deleted:    tests/132_unreachable_after_continue.my
#	deleted:    tests/133_for_cond_not_bool_para.my
#	deleted:    tests/140_param_array_1d_sum_ok.my
#	deleted:    tests/141_param_array_2d_sum_ok.my
#	deleted:    tests/142_param_array_nested_calls_ok.my
#	deleted:    tests/150_param_array_arg_scalar_fail.my
#	deleted:    tests/151_param_array_rank_mismatch_fail.my
#	deleted:    tests/152_return_array_disallowed.my
#	deleted:    tests/160_slice_1d_param_ok.my
#	deleted:    tests/161_slice_chain_ok.my
#	deleted:    tests/162_slice_index_ok.my
#	deleted:    tests/163_slice_as_scalar_op_fail.my
#	deleted:    tests/164_slice_assign_fail.my
#	deleted:    tests/165_slice_rank_mismatch_fail.my
#	deleted:    tests/166_slice_3d_2d_ok.my
#	deleted:    tests/167_slice_3d_to_1d_ok.my
#	deleted:    tests/168_slice_chain_scalar_ok.my
#	deleted:    tests/169_slice_nd_as_scalar_op_fail.my
#	deleted:    tests/170_slice_rank_mismatch_fail.my
#	deleted:    tests/171_slice_assign_nd_fail.my
#	deleted:    tests/23_copy2d_ok.my
#	deleted:    tests/23_copy_row_ok.my
#	deleted:    tests/23_do_while_simple.my
#	deleted:    tests/24_do_while_runs_once.my
#	deleted:    tests/24_fill2d_ok.my
#	deleted:    tests/24_fill_row_ok.my
#	deleted:    tests/25_copy2d_rowwide_ok.my
#	deleted:    tests/25_copy_col_ok.my
#	deleted:    tests/25_do_while_nested.my
#	deleted:    tests/26_fill2d_zero_ok.my
#	deleted:    tests/26_fill_col_ok.my
#	deleted:    tests/27_copy2d_wholeblock_ok.my
#	deleted:    tests/27_slice_assign_sugar_row_ok.my
#	deleted:    tests/28_copy2d_aligned_subrows_ok.my
#	deleted:    tests/28_transpose_col_copy_ok.my
#	deleted:    tests/29_slice_step2_ok.my
#	deleted:    tests/30_slice_fill_ok.my
#	deleted:    tests/31_runtime_fastpath_memcpy_ok.my
#	deleted:    tests/32_runtime_slow_unroll_ok.my
#	deleted:    tests/33_transpose_fast_slow_mix_ok.my
#	deleted:    tests/44_switch_simple.my
#	deleted:    tests/45_switch_default.my
#	deleted:    tests/46_switch_multi_cases.my
#	deleted:    tests/47_switch_nested.my
#	deleted:    tests/48_break_in_while.my
#	deleted:    tests/49_break_in_switch.my
#	deleted:    tests/50_fallthrough_to_next_case.my
#	deleted:    tests/51_fallthrough_into_default.my
#	deleted:    tests/52_nested_breaks.my
#	deleted:    tests/53_continue_simple.my
#	deleted:    tests/54_continue_first_stmt.my
#	deleted:    tests/55_continue_nested_with_break.my
#	deleted:    tests/56_for_sum_skip_even_para.my
#	deleted:    tests/57_for_no_init_inc_ok_para.my
#	deleted:    tests/58_for_break_and_continue_para.my
#
# Untracked files:
#	build/.cmake/
#
