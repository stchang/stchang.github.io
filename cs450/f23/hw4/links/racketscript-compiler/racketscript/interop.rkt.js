import * as $rjs_core from '../../../runtime/core.js';import * as M0 from "../../../runtime/kernel.rkt.js";import * as M1 from "../../../collects/racket/private/reverse.rkt.js";import * as M2 from "./private/interop.rkt.js";var __eq__gt_$ = function(lam_expr3795) {return $rjs_core.Marks.wrapWithContext(lam_expr3795);};var js_string = function(e3796) {return e3796.toString();};var js_string__gt_string = function(e3797) {return $rjs_core.UString.makeImmutable(e3797);};var js_array__gt_list = function(e3798) {return $rjs_core.Pair.listFromArray(e3798);};var assoc__gt_object = function(pairs3799) {var result3800 = {};var loop3801 = function(_pairs38025467) {lambda_start5466:while (true){let pairs3802 = _pairs38025467;if (M0.null_p(pairs3802)!==false) {return result3800;} else {var p3803 = M0.car(pairs3802);var k3805 = M0.car(p3803);var or_part3806 = typeof(k3805)==="string";if (or_part3806!==false) {var if_res5445 = or_part3806;} else {var if_res5445 = M0.string_p(k3805);}if (if_res5445!==false) {var if_res5447 = k3805;} else {if (M0.symbol_p(k3805)!==false) {var if_res5446 = M0.symbol__gt_string(k3805);} else {var if_res5446 = M0.error($rjs_core.PrimitiveSymbol.make("assoc->object"),$rjs_core.UString.make("invalid key value"));}var if_res5447 = if_res5446;}var key3804 = if_res5447;result3800[key3804] = M0.car(M0.cdr(p3803));_pairs38025467 = M0.cdr(pairs3802);continue lambda_start5466;}}};return loop3801(pairs3799);};var js_array_p = function(v3807) {return Array.isArray(v3807);};var in_js_array = function(arr3808) {check_array(arr3808);var arr3809 = arr3808;if (js_array_p(arr3809)!==false) {var if_res5449 = M0.rvoid();} else {var if_res5449 = in_js_array(arr3809);}if_res5449;var for_loop3810 = function(_fold_var38115471, _i38125472) {lambda_start5470:while (true){let fold_var3811 = _fold_var38115471;let i3812 = _i38125472;if (M0.__lt_(i3812,arr3809.length)!==false) {var v3813 = arr3809[i3812];if (true!==false) {var fold_var3815 = fold_var3811;var fold_var3816 = M0.cons(v3813,fold_var3815);var fold_var3814 = M0.values(fold_var3816);if (true!==false) {var if_res5450 = M0.not(false);} else {var if_res5450 = false;}if (if_res5450!==false) {_fold_var38115471 = fold_var3814;_i38125472 = i3812+1;continue lambda_start5470;} else {return fold_var3814;}} else {return fold_var3811;}} else {return fold_var3811;}}};return M1.alt_reverse(for_loop3810(M0.rnull,0));};var check_array = function(v3817) {if (js_array_p(v3817)!==false) {return M0.rvoid();} else {return M0.raise_argument_error($rjs_core.PrimitiveSymbol.make("in-js-array"),$rjs_core.UString.make("js-array?"),v3817);}};var in_js_obect = function(obj3818) {check_object(obj3818);var obj3819 = obj3818;var keys3820 = Object.keys(obj3818);if (js_object_p(obj3819)!==false) {var if_res5455 = M0.rvoid();} else {var if_res5455 = in_js_array(obj3819);}if_res5455;var for_loop3821 = function(_fold_var38225476, _i38235477) {lambda_start5475:while (true){let fold_var3822 = _fold_var38225476;let i3823 = _i38235477;if (M0.__lt_(i3823,keys3820.length)!==false) {var k3824 = keys3820[i3823];var v3825 = obj3819[keys3820[i3823]];if (true!==false) {var fold_var3827 = fold_var3822;var fold_var3828 = M0.cons(M0.values(k3824,v3825),fold_var3827);var fold_var3826 = M0.values(fold_var3828);if (true!==false) {var if_res5456 = M0.not(false);} else {var if_res5456 = false;}if (if_res5456!==false) {_fold_var38225476 = fold_var3826;_i38235477 = i3823+1;continue lambda_start5475;} else {return fold_var3826;}} else {return fold_var3822;}} else {return fold_var3822;}}};return M1.alt_reverse(for_loop3821(M0.rnull,0));};var js_object_p = function(v3829) {return ((typeof(v3829)==="object")&&(v3829!==null))&&M0.not($rjs_core.Primitive.check(v3829));};var check_object = function(v3830) {if (js_object_p(v3830)!==false) {return M0.rvoid();} else {return M0.raise_argument_error($rjs_core.PrimitiveSymbol.make("in-js-object"),$rjs_core.UString.make("js-object?"),v3830);}};var __rjs_quoted__ = {};__rjs_quoted__.js_object_p = js_object_p;__rjs_quoted__.js_array_p = js_array_p;__rjs_quoted__.in_js_array = in_js_array;__rjs_quoted__.js_string = js_string;export { __rjs_quoted__,js_object_p,js_array_p,assoc__gt_object,js_array__gt_list,js_string__gt_string,js_string,__eq__gt_$ };