import * as $rjs_core from './core.js';import * as M0 from "./lib.rkt.js";import * as M1 from "./kernel.rkt.js";import * as M2 from "../links/racketscript-compiler/racketscript/compiler/directive.rkt.js";var fl_times_ = M0.Core.attachProcedureArity(M0.Core.Number.JS.mul,0);var fl_by_ = M0.Core.attachProcedureArity(M0.Core.Number.JS.div,1);var fl_plus_ = M0.Core.attachProcedureArity(M0.Core.Number.JS.add,0);var fl_ = M0.Core.attachProcedureArity(M0.Core.Number.JS.sub,1);var fl_lt_ = M0.Core.attachProcedureArity(M0.Core.Number.JS.lt,1);var fl_gt_ = M0.Core.attachProcedureArity(M0.Core.Number.JS.gt,1);var fl_lt__eq_ = M0.Core.attachProcedureArity(M0.Core.Number.JS.lte,1);var fl_gt__eq_ = M0.Core.attachProcedureArity(M0.Core.Number.JS.gte,1);var fl_eq_ = M0.Core.attachProcedureArity(M0.Core.Number.JS.equals,1);var flabs = Math.abs;var flmin = Math.min;var flmax = Math.max;var flround = Math.round;var flfloor = Math.floor;var flceiling = Math.ceil;var fltruncate = Math.trunc;var flsin = Math.sin;var flcos = Math.cos;var fltan = Math.tan;var flasin = Math.asin;var flacos = Math.acos;var flatan = Math.atan;var fllog = Math.log;var flexp = Math.exp;var flsqrt = Math.sqrt;var flexpt = Math.pow;var fx_plus_ = function(a189, b190) {return (a189+b190)|0;};var fx_ = function(a191, b192) {return (a191-b192)|0;};var fx_times_ = function(a193, b194) {return (a193*b194)|0;};var fxquotient = function(a195, b196) {return (a195/b196)|0;};var fxremainder = function(a197, b198) {return (a197%b198)|0;};var fxmodulo = function(a199, b200) {var remainder201 = a199%b200;if ((remainder201>=0)!==false) {var if_res227 = remainder201;} else {var if_res227 = remainder201+b200;}return Math.floor(if_res227);};var fxabs = function(a202) {return Math.abs(a202);};var fx_eq_ = function(a203, b204) {return a203===b204;};var fx_lt_ = function(a205, b206) {return a205<b206;};var fx_lt__eq_ = function(a207, b208) {return a207<=b208;};var fx_gt_ = function(a209, b210) {return a209>b210;};var fx_gt__eq_ = function(a211, b212) {return a211>=b212;};var fxmin = function(a213, b214) {if ((a213<b214)!==false) {return a213;} else {return b214;}};var fxmax = function(a215, b216) {if ((a215>b216)!==false) {return b216;} else {return a215;}};var fxrshift = function(a217, b218) {return (a217>>b218)|0;};var fxlshift = function(a219, b220) {return (a219<<b220)|0;};var fxand = function(a221, b222) {return (a221&&b222)|0;};var fxior = function(a223, b224) {return (a223||b224)|0;};var fxxor = function(a225, b226) {return (a225^b226)|0;};var fxnot = M0.Core.bitwiseNot;var flvector = Array.from;var flvector_p = Array.isArray;var fxvector = Array.from;var fxvector_p = Array.isArray;var __rjs_quoted__ = {};export { __rjs_quoted__,fl_times_,fl_by_,fl_plus_,fl_,fl_lt_,fl_gt_,fl_lt__eq_,fl_gt__eq_,fl_eq_,flabs,flmin,flmax,flround,flfloor,flceiling,fltruncate,flsin,flcos,fltan,flasin,flacos,flatan,fllog,flexp,flsqrt,flexpt,fx_plus_,fx_,fx_times_,fxquotient,fxremainder,fxmodulo,fxabs,fx_eq_,fx_lt_,fx_lt__eq_,fx_gt_,fx_gt__eq_,fxmin,fxmax,fxrshift,fxlshift,fxand,fxior,fxxor,fxnot,flvector,flvector_p,fxvector,fxvector_p };