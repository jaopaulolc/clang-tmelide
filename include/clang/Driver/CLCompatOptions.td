//===--- CLCompatOptions.td - Options for clang-cl ------------------------===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//
//
//  This file defines the options accepted by clang-cl.
//
//===----------------------------------------------------------------------===//

def cl_Group : OptionGroup<"<clang-cl options>">, Flags<[CLOption]>,
  HelpText<"CL.EXE COMPATIBILITY OPTIONS">;

def cl_compile_Group : OptionGroup<"<clang-cl compile-only options>">,
  Group<cl_Group>;

def cl_ignored_Group : OptionGroup<"<clang-cl ignored options>">,
  Group<cl_Group>;

class CLFlag<string name> : Option<["/", "-"], name, KIND_FLAG>,
  Group<cl_Group>, Flags<[CLOption, DriverOption]>;

class CLCompileFlag<string name> : Option<["/", "-"], name, KIND_FLAG>,
  Group<cl_compile_Group>, Flags<[CLOption, DriverOption]>;

class CLIgnoredFlag<string name> : Option<["/", "-"], name, KIND_FLAG>,
  Group<cl_ignored_Group>, Flags<[CLOption, DriverOption]>;

class CLJoined<string name> : Option<["/", "-"], name, KIND_JOINED>,
  Group<cl_Group>, Flags<[CLOption, DriverOption]>;

class CLCompileJoined<string name> : Option<["/", "-"], name, KIND_JOINED>,
  Group<cl_compile_Group>, Flags<[CLOption, DriverOption]>;

class CLIgnoredJoined<string name> : Option<["/", "-"], name, KIND_JOINED>,
  Group<cl_ignored_Group>, Flags<[CLOption, DriverOption, HelpHidden]>;

class CLJoinedOrSeparate<string name> : Option<["/", "-"], name,
  KIND_JOINED_OR_SEPARATE>, Group<cl_Group>, Flags<[CLOption, DriverOption]>;

class CLCompileJoinedOrSeparate<string name> : Option<["/", "-"], name,
  KIND_JOINED_OR_SEPARATE>, Group<cl_compile_Group>,
  Flags<[CLOption, DriverOption]>;

class CLRemainingArgsJoined<string name> : Option<["/", "-"], name,
  KIND_REMAINING_ARGS_JOINED>, Group<cl_Group>, Flags<[CLOption, DriverOption]>;

// Aliases:
// (We don't put any of these in cl_compile_Group as the options they alias are
// already in the right group.)

def _SLASH_Brepro : CLFlag<"Brepro">,
  HelpText<"Do not write current time into COFF output (breaks link.exe /incremental)">,
  Alias<mno_incremental_linker_compatible>;
def _SLASH_Brepro_ : CLFlag<"Brepro-">,
  HelpText<"Write current time into COFF output (default)">,
  Alias<mincremental_linker_compatible>;
def _SLASH_C : CLFlag<"C">,
  HelpText<"Do not discard comments when preprocessing">, Alias<C>;
def _SLASH_c : CLFlag<"c">, HelpText<"Compile only">, Alias<c>;
def _SLASH_d1PP : CLFlag<"d1PP">,
  HelpText<"Retain macro definitions in /E mode">, Alias<dD>;
def _SLASH_d1reportAllClassLayout : CLFlag<"d1reportAllClassLayout">,
  HelpText<"Dump record layout information">,
  Alias<Xclang>, AliasArgs<["-fdump-record-layouts"]>;
def _SLASH_diagnostics_caret : CLFlag<"diagnostics:caret">,
  HelpText<"Enable caret and column diagnostics (default)">;
def _SLASH_diagnostics_column : CLFlag<"diagnostics:column">,
  HelpText<"Disable caret diagnostics but keep column info">;
def _SLASH_diagnostics_classic : CLFlag<"diagnostics:classic">,
  HelpText<"Disable column and caret diagnostics">;
def _SLASH_D : CLJoinedOrSeparate<"D">, HelpText<"Define macro">,
  MetaVarName<"<macro[=value]>">, Alias<D>;
def _SLASH_E : CLFlag<"E">, HelpText<"Preprocess to stdout">, Alias<E>;
def _SLASH_fp_except : CLFlag<"fp:except">, HelpText<"">, Alias<ftrapping_math>;
def _SLASH_fp_except_ : CLFlag<"fp:except-">,
  HelpText<"">, Alias<fno_trapping_math>;
def _SLASH_fp_fast : CLFlag<"fp:fast">, HelpText<"">, Alias<ffast_math>;
def _SLASH_fp_precise : CLFlag<"fp:precise">,
  HelpText<"">, Alias<fno_fast_math>;
def _SLASH_fp_strict : CLFlag<"fp:strict">, HelpText<"">, Alias<fno_fast_math>;
def _SLASH_GA : CLFlag<"GA">, Alias<ftlsmodel_EQ>, AliasArgs<["local-exec"]>,
  HelpText<"Assume thread-local variables are defined in the executable">;
def _SLASH_GR : CLFlag<"GR">, HelpText<"Emit RTTI data (default)">;
def _SLASH_GR_ : CLFlag<"GR-">, HelpText<"Do not emit RTTI data">;
def _SLASH_GF : CLIgnoredFlag<"GF">,
  HelpText<"Enable string pooling (default)">;
def _SLASH_GF_ : CLFlag<"GF-">, HelpText<"Disable string pooling">,
  Alias<fwritable_strings>;
def _SLASH_GS : CLFlag<"GS">,
  HelpText<"Enable buffer security check (default)">;
def _SLASH_GS_ : CLFlag<"GS-">, HelpText<"Disable buffer security check">;
def : CLFlag<"Gs">, HelpText<"Use stack probes (default)">,
  Alias<mstack_probe_size>, AliasArgs<["4096"]>;
def _SLASH_Gs : CLJoined<"Gs">,
  HelpText<"Set stack probe size (default 4096)">, Alias<mstack_probe_size>;
def _SLASH_Gy : CLFlag<"Gy">, HelpText<"Put each function in its own section">,
  Alias<ffunction_sections>;
def _SLASH_Gy_ : CLFlag<"Gy-">,
  HelpText<"Do not put each function in its own section (default)">,
  Alias<fno_function_sections>;
def _SLASH_Gw : CLFlag<"Gw">, HelpText<"Put each data item in its own section">,
  Alias<fdata_sections>;
def _SLASH_Gw_ : CLFlag<"Gw-">,
  HelpText<"Do not put each data item in its own section (default)">,
  Alias<fno_data_sections>;
def _SLASH_help : CLFlag<"help">, Alias<help>,
  HelpText<"Display available options">;
def _SLASH_HELP : CLFlag<"HELP">, Alias<help>;
def _SLASH_I : CLJoinedOrSeparate<"I">,
  HelpText<"Add directory to include search path">, MetaVarName<"<dir>">,
  Alias<I>;
def _SLASH_J : CLFlag<"J">, HelpText<"Make char type unsigned">,
  Alias<funsigned_char>;

// The _SLASH_O option handles all the /O flags, but we also provide separate
// aliased options to provide separate help messages.
def _SLASH_O : CLJoined<"O">,
  HelpText<"Set multiple /O flags at once; e.g. '/O2y-' for '/O2 /Oy-'">,
  MetaVarName<"<flags>">;
def : CLFlag<"O1">, Alias<_SLASH_O>, AliasArgs<["1"]>,
  HelpText<"Optimize for size  (like /Og     /Os /Oy /Ob2 /GF /Gy)">;
def : CLFlag<"O2">, Alias<_SLASH_O>, AliasArgs<["2"]>,
  HelpText<"Optimize for speed (like /Og /Oi /Ot /Oy /Ob2 /GF /Gy)">;
def : CLFlag<"Ob0">, Alias<_SLASH_O>, AliasArgs<["b0"]>,
  HelpText<"Disable function inlining">;
def : CLFlag<"Ob1">, Alias<_SLASH_O>, AliasArgs<["b1"]>,
  HelpText<"Only inline functions explicitly or implicitly marked inline">;
def : CLFlag<"Ob2">, Alias<_SLASH_O>, AliasArgs<["b2"]>,
  HelpText<"Inline functions as deemed beneficial by the compiler">;
def : CLFlag<"Od">, Alias<_SLASH_O>, AliasArgs<["d"]>,
  HelpText<"Disable optimization">;
def : CLFlag<"Og">, Alias<_SLASH_O>, AliasArgs<["g"]>,
  HelpText<"No effect">;
def : CLFlag<"Oi">, Alias<_SLASH_O>, AliasArgs<["i"]>,
  HelpText<"Enable use of builtin functions">;
def : CLFlag<"Oi-">, Alias<_SLASH_O>, AliasArgs<["i-"]>,
  HelpText<"Disable use of builtin functions">;
def : CLFlag<"Os">, Alias<_SLASH_O>, AliasArgs<["s"]>,
  HelpText<"Optimize for size">;
def : CLFlag<"Ot">, Alias<_SLASH_O>, AliasArgs<["t"]>,
  HelpText<"Optimize for speed">;
def : CLFlag<"Ox">, Alias<_SLASH_O>, AliasArgs<["x"]>,
  HelpText<"Deprecated (like /Og /Oi /Ot /Oy /Ob2); use /O2">;
def : CLFlag<"Oy">, Alias<_SLASH_O>, AliasArgs<["y"]>,
  HelpText<"Enable frame pointer omission (x86 only)">;
def : CLFlag<"Oy-">, Alias<_SLASH_O>, AliasArgs<["y-"]>,
  HelpText<"Disable frame pointer omission (x86 only, default)">;

def _SLASH_QUESTION : CLFlag<"?">, Alias<help>,
  HelpText<"Display available options">;
def _SLASH_Qvec : CLFlag<"Qvec">,
  HelpText<"Enable the loop vectorization passes">, Alias<fvectorize>;
def _SLASH_Qvec_ : CLFlag<"Qvec-">,
  HelpText<"Disable the loop vectorization passes">, Alias<fno_vectorize>;
def _SLASH_showIncludes : CLFlag<"showIncludes">,
  HelpText<"Print info about included files to stderr">,
  Alias<show_includes>;
def _SLASH_showFilenames : CLFlag<"showFilenames">,
  HelpText<"Print the name of each compiled file">;
def _SLASH_showFilenames_ : CLFlag<"showFilenames-">,
  HelpText<"Do not print the name of each compiled file (default)">;
def _SLASH_source_charset : CLCompileJoined<"source-charset:">,
  HelpText<"Set source encoding, supports only UTF-8">,
  Alias<finput_charset_EQ>;
def _SLASH_execution_charset : CLCompileJoined<"execution-charset:">,
  HelpText<"Set runtime encoding, supports only UTF-8">,
  Alias<fexec_charset_EQ>;
def _SLASH_std : CLCompileJoined<"std:">,
  HelpText<"Set C++ version (c++14,c++17,c++latest)">;
def _SLASH_U : CLJoinedOrSeparate<"U">, HelpText<"Undefine macro">,
  MetaVarName<"<macro>">, Alias<U>;
def _SLASH_validate_charset : CLFlag<"validate-charset">,
  Alias<W_Joined>, AliasArgs<["invalid-source-encoding"]>;
def _SLASH_validate_charset_ : CLFlag<"validate-charset-">,
  Alias<W_Joined>, AliasArgs<["no-invalid-source-encoding"]>;
def _SLASH_W0 : CLFlag<"W0">, HelpText<"Disable all warnings">, Alias<w>;
def _SLASH_W1 : CLFlag<"W1">, HelpText<"Enable -Wall">, Alias<Wall>;
def _SLASH_W2 : CLFlag<"W2">, HelpText<"Enable -Wall">, Alias<Wall>;
def _SLASH_W3 : CLFlag<"W3">, HelpText<"Enable -Wall">, Alias<Wall>;
def _SLASH_W4 : CLFlag<"W4">, HelpText<"Enable -Wall and -Wextra">, Alias<WCL4>;
def _SLASH_Wall : CLFlag<"Wall">, HelpText<"Enable -Weverything">,
  Alias<W_Joined>, AliasArgs<["everything"]>;
def _SLASH_WX : CLFlag<"WX">, HelpText<"Treat warnings as errors">,
  Alias<W_Joined>, AliasArgs<["error"]>;
def _SLASH_WX_ : CLFlag<"WX-">,
  HelpText<"Do not treat warnings as errors (default)">,
  Alias<W_Joined>, AliasArgs<["no-error"]>;
def _SLASH_w_flag : CLFlag<"w">, HelpText<"Disable all warnings">, Alias<w>;
def _SLASH_wd4005 : CLFlag<"wd4005">, Alias<W_Joined>,
  AliasArgs<["no-macro-redefined"]>;
def _SLASH_wd4018 : CLFlag<"wd4018">, Alias<W_Joined>,
  AliasArgs<["no-sign-compare"]>;
def _SLASH_wd4100 : CLFlag<"wd4100">, Alias<W_Joined>,
  AliasArgs<["no-unused-parameter"]>;
def _SLASH_wd4910 : CLFlag<"wd4910">, Alias<W_Joined>,
  AliasArgs<["no-dllexport-explicit-instantiation-decl"]>;
def _SLASH_wd4996 : CLFlag<"wd4996">, Alias<W_Joined>,
  AliasArgs<["no-deprecated-declarations"]>;
def _SLASH_vd : CLJoined<"vd">, HelpText<"Control vtordisp placement">,
  Alias<vtordisp_mode_EQ>;
def _SLASH_X : CLFlag<"X">,
  HelpText<"Do not add %INCLUDE% to include search path">, Alias<nostdlibinc>;
def _SLASH_Zc_sizedDealloc : CLFlag<"Zc:sizedDealloc">,
  HelpText<"Enable C++14 sized global deallocation functions">,
  Alias<fsized_deallocation>;
def _SLASH_Zc_sizedDealloc_ : CLFlag<"Zc:sizedDealloc-">,
  HelpText<"Disable C++14 sized global deallocation functions">,
  Alias<fno_sized_deallocation>;
def _SLASH_Zc_alignedNew : CLFlag<"Zc:alignedNew">,
  HelpText<"Enable C++17 aligned allocation functions">,
  Alias<faligned_allocation>;
def _SLASH_Zc_alignedNew_ : CLFlag<"Zc:alignedNew-">,
  HelpText<"Disable C++17 aligned allocation functions">,
  Alias<fno_aligned_allocation>;
def _SLASH_Zc_char8_t : CLFlag<"Zc:char8_t">,
  HelpText<"Enable char8_t from C++2a">,
  Alias<fchar8__t>;
def _SLASH_Zc_char8_t_ : CLFlag<"Zc:char8_t-">,
  HelpText<"Disable char8_t from c++2a">,
  Alias<fno_char8__t>;
def _SLASH_Zc_strictStrings : CLFlag<"Zc:strictStrings">,
  HelpText<"Treat string literals as const">, Alias<W_Joined>,
  AliasArgs<["error=c++11-compat-deprecated-writable-strings"]>;
def _SLASH_Zc_threadSafeInit : CLFlag<"Zc:threadSafeInit">,
  HelpText<"Enable thread-safe initialization of static variables">,
  Alias<fthreadsafe_statics>;
def _SLASH_Zc_threadSafeInit_ : CLFlag<"Zc:threadSafeInit-">,
  HelpText<"Disable thread-safe initialization of static variables">,
  Alias<fno_threadsafe_statics>;
def _SLASH_Zc_trigraphs : CLFlag<"Zc:trigraphs">,
  HelpText<"Enable trigraphs">, Alias<ftrigraphs>;
def _SLASH_Zc_trigraphs_off : CLFlag<"Zc:trigraphs-">,
  HelpText<"Disable trigraphs (default)">, Alias<fno_trigraphs>;
def _SLASH_Zc_twoPhase : CLFlag<"Zc:twoPhase">,
  HelpText<"Enable two-phase name lookup in templates (default)">,
  Alias<fno_delayed_template_parsing>;
def _SLASH_Zc_twoPhase_ : CLFlag<"Zc:twoPhase-">,
  HelpText<"Disable two-phase name lookup in templates">,
  Alias<fdelayed_template_parsing>;
def _SLASH_Z7 : CLFlag<"Z7">,
  HelpText<"Enable CodeView debug information in object files">;
def _SLASH_Zd : CLFlag<"Zd">,
  HelpText<"Emit debug line number tables only">;
def _SLASH_Zi : CLFlag<"Zi">, Alias<_SLASH_Z7>,
  HelpText<"Like /Z7">;
def _SLASH_Zp : CLJoined<"Zp">,
  HelpText<"Set default maximum struct packing alignment">,
  Alias<fpack_struct_EQ>;
def _SLASH_Zp_flag : CLFlag<"Zp">,
  HelpText<"Set default maximum struct packing alignment to 1">,
  Alias<fpack_struct_EQ>, AliasArgs<["1"]>;
def _SLASH_Zs : CLFlag<"Zs">, HelpText<"Syntax-check only">,
  Alias<fsyntax_only>;
def _SLASH_openmp_ : CLFlag<"openmp-">,
  HelpText<"Disable OpenMP support">, Alias<fno_openmp>;
def _SLASH_openmp : CLFlag<"openmp">, HelpText<"Enable OpenMP support">,
  Alias<fopenmp>;
def _SLASH_openmp_experimental : CLFlag<"openmp:experimental">,
  HelpText<"Enable OpenMP support with experimental SIMD support">,
  Alias<fopenmp>;

// Non-aliases:

def _SLASH_arch : CLCompileJoined<"arch:">,
  HelpText<"Set architecture for code generation">;

def _SLASH_M_Group : OptionGroup<"</M group>">, Group<cl_compile_Group>;
def _SLASH_volatile_Group : OptionGroup<"</volatile group>">,
  Group<cl_compile_Group>;

def _SLASH_EH : CLJoined<"EH">, HelpText<"Set exception handling model">;
def _SLASH_EP : CLFlag<"EP">,
  HelpText<"Disable linemarker output and preprocess to stdout">;
def _SLASH_FA : CLFlag<"FA">,
  HelpText<"Output assembly code file during compilation">;
def _SLASH_Fa : CLJoined<"Fa">,
  HelpText<"Set assembly output file name (with /FA)">,
  MetaVarName<"<file or dir/>">;
def _SLASH_fallback : CLCompileFlag<"fallback">,
  HelpText<"Fall back to cl.exe if clang-cl fails to compile">;
def _SLASH_FI : CLJoinedOrSeparate<"FI">,
  HelpText<"Include file before parsing">, Alias<include_>;
def _SLASH_Fe : CLJoined<"Fe">,
  HelpText<"Set output executable file name">,
  MetaVarName<"<file or dir/>">;
def _SLASH_Fi : CLCompileJoined<"Fi">,
  HelpText<"Set preprocess output file name (with /P)">,
  MetaVarName<"<file>">;
def _SLASH_Fo : CLCompileJoined<"Fo">,
  HelpText<"Set output object file (with /c)">,
  MetaVarName<"<file or dir/>">;
def _SLASH_guard : CLJoined<"guard:">,
  HelpText<"Enable Control Flow Guard with /guard:cf, or only the table with /guard:cf,nochecks">;
def _SLASH_GX : CLFlag<"GX">,
  HelpText<"Deprecated; use /EHsc">;
def _SLASH_GX_ : CLFlag<"GX-">,
  HelpText<"Deprecated (like not passing /EH)">;
def _SLASH_imsvc : CLJoinedOrSeparate<"imsvc">,
  HelpText<"Add <dir> to system include search path, as if in %INCLUDE%">,
  MetaVarName<"<dir>">;
def _SLASH_LD : CLFlag<"LD">, HelpText<"Create DLL">;
def _SLASH_LDd : CLFlag<"LDd">, HelpText<"Create debug DLL">;
def _SLASH_link : CLRemainingArgsJoined<"link">,
  HelpText<"Forward options to the linker">, MetaVarName<"<options>">;
def _SLASH_MD : Option<["/", "-"], "MD", KIND_FLAG>, Group<_SLASH_M_Group>,
  Flags<[CLOption, DriverOption]>, HelpText<"Use DLL run-time">;
def _SLASH_MDd : Option<["/", "-"], "MDd", KIND_FLAG>, Group<_SLASH_M_Group>,
  Flags<[CLOption, DriverOption]>, HelpText<"Use DLL debug run-time">;
def _SLASH_MT : Option<["/", "-"], "MT", KIND_FLAG>, Group<_SLASH_M_Group>,
  Flags<[CLOption, DriverOption]>, HelpText<"Use static run-time">;
def _SLASH_MTd : Option<["/", "-"], "MTd", KIND_FLAG>, Group<_SLASH_M_Group>,
  Flags<[CLOption, DriverOption]>, HelpText<"Use static debug run-time">;
def _SLASH_o : CLJoinedOrSeparate<"o">,
  HelpText<"Deprecated (set output file name); use /Fe or /Fe">,
  MetaVarName<"<file or dir/>">;
def _SLASH_P : CLFlag<"P">, HelpText<"Preprocess to file">;
def _SLASH_Tc : CLCompileJoinedOrSeparate<"Tc">,
  HelpText<"Treat <file> as C source file">, MetaVarName<"<file>">;
def _SLASH_TC : CLCompileFlag<"TC">, HelpText<"Treat all source files as C">;
def _SLASH_Tp : CLCompileJoinedOrSeparate<"Tp">,
  HelpText<"Treat <file> as C++ source file">, MetaVarName<"<file>">;
def _SLASH_TP : CLCompileFlag<"TP">, HelpText<"Treat all source files as C++">;
def _SLASH_volatile_iso : Option<["/", "-"], "volatile:iso", KIND_FLAG>,
  Group<_SLASH_volatile_Group>, Flags<[CLOption, DriverOption]>,
  HelpText<"Volatile loads and stores have standard semantics">;
def _SLASH_vmb : CLFlag<"vmb">,
  HelpText<"Use a best-case representation method for member pointers">;
def _SLASH_vmg : CLFlag<"vmg">,
  HelpText<"Use a most-general representation for member pointers">;
def _SLASH_vms : CLFlag<"vms">,
  HelpText<"Set the default most-general representation to single inheritance">;
def _SLASH_vmm : CLFlag<"vmm">,
  HelpText<"Set the default most-general representation to "
           "multiple inheritance">;
def _SLASH_vmv : CLFlag<"vmv">,
  HelpText<"Set the default most-general representation to "
           "virtual inheritance">;
def _SLASH_volatile_ms  : Option<["/", "-"], "volatile:ms", KIND_FLAG>,
  Group<_SLASH_volatile_Group>, Flags<[CLOption, DriverOption]>,
  HelpText<"Volatile loads and stores have acquire and release semantics">;
def _SLASH_clang : CLJoined<"clang:">,
  HelpText<"Pass <arg> to the clang driver">, MetaVarName<"<arg>">;
def _SLASH_Zl : CLFlag<"Zl">,
  HelpText<"Do not let object file auto-link default libraries">;

def _SLASH_Yc : CLJoined<"Yc">,
  HelpText<"Generate a pch file for all code up to and including <filename>">,
  MetaVarName<"<filename>">;
def _SLASH_Yu : CLJoined<"Yu">,
  HelpText<"Load a pch file and use it instead of all code up to "
           "and including <filename>">,
  MetaVarName<"<filename>">;
def _SLASH_Y_ : CLFlag<"Y-">,
  HelpText<"Disable precompiled headers, overrides /Yc and /Yu">;
def _SLASH_Zc_dllexportInlines : CLFlag<"Zc:dllexportInlines">,
  HelpText<"dllexport/dllimport inline member functions of dllexport/import classes (default)">;
def _SLASH_Zc_dllexportInlines_ : CLFlag<"Zc:dllexportInlines-">,
  HelpText<"Do not dllexport/dllimport inline member functions of dllexport/import classes">;
def _SLASH_Fp : CLJoined<"Fp">,
  HelpText<"Set pch file name (with /Yc and /Yu)">, MetaVarName<"<file>">;

def _SLASH_Gd : CLFlag<"Gd">,
  HelpText<"Set __cdecl as a default calling convention">;
def _SLASH_Gr : CLFlag<"Gr">,
  HelpText<"Set __fastcall as a default calling convention">;
def _SLASH_Gz : CLFlag<"Gz">,
  HelpText<"Set __stdcall as a default calling convention">;
def _SLASH_Gv : CLFlag<"Gv">,
  HelpText<"Set __vectorcall as a default calling convention">;
def _SLASH_Gregcall : CLFlag<"Gregcall">,
  HelpText<"Set __regcall as a default calling convention">;

// Ignored:

def _SLASH_analyze_ : CLIgnoredFlag<"analyze-">;
def _SLASH_bigobj : CLIgnoredFlag<"bigobj">;
def _SLASH_cgthreads : CLIgnoredJoined<"cgthreads">;
def _SLASH_d2FastFail : CLIgnoredFlag<"d2FastFail">;
def _SLASH_d2Zi_PLUS : CLIgnoredFlag<"d2Zi+">;
def _SLASH_errorReport : CLIgnoredJoined<"errorReport">;
def _SLASH_FC : CLIgnoredFlag<"FC">;
def _SLASH_Fd : CLIgnoredJoined<"Fd">;
def _SLASH_FS : CLIgnoredFlag<"FS">;
def _SLASH_JMC : CLIgnoredFlag<"JMC">;
def _SLASH_kernel_ : CLIgnoredFlag<"kernel-">;
def _SLASH_nologo : CLIgnoredFlag<"nologo">;
def _SLASH_permissive_ : CLIgnoredFlag<"permissive-">;
def _SLASH_RTC : CLIgnoredJoined<"RTC">;
def _SLASH_sdl : CLIgnoredFlag<"sdl">;
def _SLASH_sdl_ : CLIgnoredFlag<"sdl-">;
def _SLASH_utf8 : CLIgnoredFlag<"utf-8">,
  HelpText<"Set source and runtime encoding to UTF-8 (default)">;
def _SLASH_w : CLIgnoredJoined<"w">;
def _SLASH_Zc___cplusplus : CLIgnoredFlag<"Zc:__cplusplus">;
def _SLASH_Zc_auto : CLIgnoredFlag<"Zc:auto">;
def _SLASH_Zc_forScope : CLIgnoredFlag<"Zc:forScope">;
def _SLASH_Zc_inline : CLIgnoredFlag<"Zc:inline">;
def _SLASH_Zc_rvalueCast : CLIgnoredFlag<"Zc:rvalueCast">;
def _SLASH_Zc_ternary : CLIgnoredFlag<"Zc:ternary">;
def _SLASH_Zc_wchar_t : CLIgnoredFlag<"Zc:wchar_t">;
def _SLASH_Zm : CLIgnoredJoined<"Zm">;
def _SLASH_Zo : CLIgnoredFlag<"Zo">;
def _SLASH_Zo_ : CLIgnoredFlag<"Zo-">;


// Unsupported:

def _SLASH_await : CLFlag<"await">;
def _SLASH_constexpr : CLJoined<"constexpr:">;
def _SLASH_AI : CLJoinedOrSeparate<"AI">;
def _SLASH_Bt : CLFlag<"Bt">;
def _SLASH_Bt_plus : CLFlag<"Bt+">;
def _SLASH_clr : CLJoined<"clr">;
def _SLASH_d2 : CLJoined<"d2">;
def _SLASH_doc : CLJoined<"doc">;
def _SLASH_FA_joined : CLJoined<"FA">;
def _SLASH_favor : CLJoined<"favor">;
def _SLASH_F : CLJoinedOrSeparate<"F">;
def _SLASH_Fm : CLJoined<"Fm">;
def _SLASH_Fr : CLJoined<"Fr">;
def _SLASH_FR : CLJoined<"FR">;
def _SLASH_FU : CLJoinedOrSeparate<"FU">;
def _SLASH_Fx : CLFlag<"Fx">;
def _SLASH_G1 : CLFlag<"G1">;
def _SLASH_G2 : CLFlag<"G2">;
def _SLASH_Ge : CLFlag<"Ge">;
def _SLASH_Gh : CLFlag<"Gh">;
def _SLASH_GH : CLFlag<"GH">;
def _SLASH_GL : CLFlag<"GL">;
def _SLASH_GL_ : CLFlag<"GL-">;
def _SLASH_Gm : CLFlag<"Gm">;
def _SLASH_Gm_ : CLFlag<"Gm-">;
def _SLASH_GT : CLFlag<"GT">;
def _SLASH_GZ : CLFlag<"GZ">;
def _SLASH_H : CLFlag<"H">;
def _SLASH_homeparams : CLFlag<"homeparams">;
def _SLASH_hotpatch : CLFlag<"hotpatch">;
def _SLASH_kernel : CLFlag<"kernel">;
def _SLASH_LN : CLFlag<"LN">;
def _SLASH_MP : CLJoined<"MP">;
def _SLASH_Qfast_transcendentals : CLFlag<"Qfast_transcendentals">;
def _SLASH_QIfist : CLFlag<"QIfist">;
def _SLASH_Qimprecise_fwaits : CLFlag<"Qimprecise_fwaits">;
def _SLASH_Qpar : CLFlag<"Qpar">;
def _SLASH_Qpar_report : CLJoined<"Qpar-report">;
def _SLASH_Qsafe_fp_loads : CLFlag<"Qsafe_fp_loads">;
def _SLASH_Qspectre : CLFlag<"Qspectre">;
def _SLASH_Qvec_report : CLJoined<"Qvec-report">;
def _SLASH_u : CLFlag<"u">;
def _SLASH_V : CLFlag<"V">;
def _SLASH_WL : CLFlag<"WL">;
def _SLASH_Wp64 : CLFlag<"Wp64">;
def _SLASH_Yd : CLFlag<"Yd">;
def _SLASH_Yl : CLJoined<"Yl">;
def _SLASH_Za : CLFlag<"Za">;
def _SLASH_Zc : CLJoined<"Zc:">;
def _SLASH_Ze : CLFlag<"Ze">;
def _SLASH_Zg : CLFlag<"Zg">;
def _SLASH_ZI : CLFlag<"ZI">;
def _SLASH_ZW : CLJoined<"ZW">;
