
Fortran90 package alpinf90utils
================================

This is a utility library package for Fortran 90 (in fact, the standard of Fortran 2008 is required). It should work with `gfortran` (as of Version 10.2.0).  The behaviour is checked with macOS Big Sur and Linux Ubuntu (5.8.0-48-generic).

The package consists of the following 3 directories. Each of these directories contains `Makefile` and the standard `make` should work.

1. `src` : the main source for the library
   * Additionally, `make test` compiles both the library and test source files, and runs the tests.
2. `samples` : containing a sample program
   * Additionally, `make run` compiles the main sample program and runs it.
3. `test` : test suite.
   * Additionally, `make test` compiles the test source files, and runs the tests.

## Sample program ##

Once you have compiled the source library files, you can compile and run the sample program: `sample01`. It demonstrates the functions and subroutines contained in the library. `make run` is a convenient short-cut to (re)compile the sample program (but **not** the library) if required and run it.

Note that with `make run`, it *fails* at the end of the processing with the exitstatus of 1 (which is then converted into 2 by `make` and therefore what you will see is an exitstatus of 2). That is part of the demonstration of the library.

The help is displayed with `./sample01 -h` from the command line.

The command `sample01` accepts an option `-v` or `--verbose` as the first argument and 1 main argument, but nothing more. If extra arguments are specified, it exits with exit status of 1 (as demonstrated with `make run` in its first part).

Instead of specifying the option `-v`, you can set the `VERBOSE` environmental variable to something but `no` or `false` to get the same effect.

The main PROGRAM part of `sample01.f90` demonstrates the major functions of the library. The module `m_handle_argv` contained in the same file demonstrates some of them, particularly those in `alpin_hash`. The module also serves as a demonstration of how to handle the UNIX command-line arguments with the library.

## Library ##

This package will, after `make`, contain 1 `ar`-ed library `libalpinf90utils.a` and 1 object file `alpin_unittest.o` (and associated `alpin_unittest.mod`) in `src/` directory.

The former, `libalpinf90utils.a`, contains 3 modules: 

1. `alpin_err_exit`
2. `alpin_misc_utils`
3. `alpin_hash`

The latter contains the module of the same name as the file, `alpin_unittest`, which is expected to be linked at the compile time in your test suite —— see `test/Makefile` for an example.

### Module `alpin_err_exit` ###

This module contains very basic two subroutines that print the specified message with pre-determined prefix and `exit(1)` (for UNIX). The latter is designed to be used to *play safe*, i.e., where the case should never happen in your code (namely, if it happens, it means the code behaves not in the expected way).

This library uses `call EXIT(1)`. In fact, there is no official Fortran standard (up to 2008) to do the job. However, this function seems to be the defunct standard and *gfortran* supports it.

* `err_exit_with_msg(usermsg)`
* `err_exit_play_safe(usermsg)`

### Module `alpin_misc_utils`

* Constant (kind=8): `PI`
* `real(kind=8) function rad2deg(rad)`
  * Returns the converted value from the given radian in `real(kind=8)` to degree.
* `real(kind=8) function deg2rad(deg)`
  * Returns the converted value from the given degree in `real(kind=8)` to radian.
* `character function basename(fname)`
  * `basename` of the given filename, i.e., the path-name excluding all the parent/ancestor directory parts.
  * If the `fname` is a directory, the basename is the directory name with neither the parent/ancestor directory parts nor trailing directory separator "`/`". If the `fname` is the root directory, "`/`" is returned.
  * The length of the returned `CHARACTER` is the same as that of the given `fname`.
* INTERFACE `join_ary(ary [,sep] [,quote])`
  * Returns the *joined* string of the given array, where each string is TRIM-med. For example, `join_ary(['x  ','y'], quote='$')` returns "`$x$, $y$`" (with trailing spaces).
  * The length of the returned character is specified in the (module-global) parameter `LEN_JOIN_ARY` and 4096 (at the time of writing). If the given `ary` is large to the extent that the length of the joined string would exceed the limit, the exceeded part is simply trimmed.
  * `ary` can be `CHARACTER` or `INTEGER` (kind=1,2,4,8).
  * `sep` is an optional parameter for the separator (16 characters at maximum). The default is "`,⎵`" (comma+space). Note that this argument is **not** TRIM-med. Make sure the length is what you want to be used as that of the separator.
  * `quote` is an optional parameter for the quote symbol, valid only for a character `ary`. The default is a single quote, but you may specify a double quote instead, for example.
* INTERFACE `character function ladjusted_int(i)`
  * Returns the left-adjusted form of the CHARACTER of the given INTEGER (kind=1,2,4,8).
  * The length of the returned CHARACTER is just sufficiently long to accommodate the largest negative value of the type. You can `TRIM()` it.
* INTERFACE `integer(kind=4) function unsigned1_to_int4(ibyte)`
  * Interpret the given `ibyte` of `INTEGER*1` (either a scalar or array) as the unsigned integer and returns it as `INTEGER*4`.
  * Usually, `INTEGER*1` ranges from -127 to 127. But this function interprets it ranging from 0 to 255.
* INTERFACE `integer(kind=1) function int4_to_unsigned1(i4)`
  * The reverse of `unsigned1_to_int4(ibyte)` for a scalar or array.
* INTERFACE `character function dump_int_binary(i)`
  * Returns a CHARACTER representation with bits of the given integer `i` (kind=1,2,4).  It contains an underscore (`_`) character in every 8 bits except for `INTEGER*1`.
* INTERFACE `integer function findloc1(array, value [,mask] [,back])`
  * Wrapper of the Fortran-2008 `findloc()` function, i.e., returning the first (or last) positional index of the `value` in the given `array`, but works only for 1-dimensional array and returns a scalar `INTEGER*4`.
  * `array` can be LOGICAL, CHARACTER, INTEGER, REAL with various kinds.
  * `value` is the value to search for. The type must agree with that of the `array` except for INTEGERs, in which case another type of INTEGER may be accepted.
  * `mask` is an optional `LOGICAL` array to specify the mask as in `findloc()`.
  * `back` is an optional `LOGICAL` scalar as in `findloc()`. If True, search starts from the tail in the reverse order.

### Module `alpin_hash`

This module defines an element of Perl/Ruby-type *hash* (or called dictionary in Python and associated-array in PHP) as a type that has two elements: (CHARACTER) `key` and (arbitrary-type) `val`, the latter of which can be any of LOGICAL, CHARACTER, INTEGER, and REAL with various kinds.

When the type (e.g., `t_alpin_hash_char`) is used as an Array, it emulates the hash. The major difference from a Fortran90-builtin *TYPE*, where a number of members are defined, is that this *hash*-type array allows you to specify (or browse) the key dynamically or as a set; in other words, if there is no need of flexibility of those kinds, the built-in TYPE is superior, obviously.

The most basic function of the hash is to look up a value corresponding to the specified key.  So far, this library provides the functionality of just that.

* INTERFACE `function fetch_hashval(key, ary [,undef])`
  * Function to get the value corresponding to the specified `key`.  The type of the returned value is the same as that of the specified `ary`.
  * If `ary` does not contain the specified `key`, the value of the optional argument `undef` is, if specified, returned. If not specified, the default value defined for the `type` (e.g., `t_alpin_hash_char%val`) is returned.
  * If there is no suitable `undef` value for the `ary` in your use-case and if there is a real possibility that `ary` may not contain the `key`, you should use `subroutine hashval_status()` instead, with which information of the existence of the key is returned explicitly.
* INTERFACE `subroutine hashval_status(key, ary, >val, >is_undef)`
  * This is the parent subroutine that the function `fetch_hashval()` calls internally.
  * The former two (`key`, `ary`) are `INTENT(IN)` and the latter two (`val`, `is_undef`) are `INTENT(OUT)`.
  * If the specified `key` does not exist in `ary`, `is_undef` is set True (which is otherwise False).

### Module `alpin_unittest` ###

All the functions in this library return a logical value; if *True*, it means the test succeeds, and if *False*, the test fails and an error message is printed to STDERR, telling what the expected and actual values are.

The arguments `esq` and `act` mean the *expected* and *actual* values, respectively (n.b., `esq` instead of `exp` simply because `exp` is a reserved word in Fortran).

Also, `subname` (subroutine name, or whatever) and the optional argument `optmsg` are used purely for a displaying purpose. You can specify whatever you like for them.

Unlike the standard unit-test schemes in other major languages like Ruby and Python, the functions in this library do not show the line-number of the code etc, and so it is advised to specify some helpful CHARACTER strings so that you would be able to pin down easily the location of failure if it does.

The following functions are included.
See the [test suite](./test/) of this package for sample uses of these functions (and its `Makefile` for how to link it in compiling).

* `logical function assert(res, subname [,optmsg])`
  * `res` is a logical value. This function simply returns `res`.
* `logical function assert_not(res, subname [,optmsg])`
  * Simply the reverse of `assert()`
* INTERFACE `assert_equal(exq, act, subname [,optmsg])`
  * Asserts `esq` and `act` are equal. They can be one of `CHARACTER`, `INTEGER*1`, `INTEGER*4` scalars and arrays (experimental).
  * For `REAL`, use `assert_in_delta()` instead.
* INTERFACE `assert_in_delta(exq, act, delta, subname [,optmsg])`
  * Asserts `esq` and `act` are equal within the difference `delta`. They can be either `REAL*4` or `REAL*8`.
  * Note that not all the combinations of `REAL*4` and `REAL*8` for the 3 parameters are accepted at the time of writing. So, you might need to convert a value(s) with the `REAL()` function to pass it.
* INTERFACE `assert_smaller_than(exq, act, delta, subname [,optmsg])`
  * Asserts `act` is smaller than `esq`. They can be either `REAL*4` or `REAL*8`, but their types must agree (at the time of writing).
* INTERFACE `assert_greater_than(exq, act, delta, subname [,optmsg])`
  * Asserts `act` is greater than `esq`. They can be either `REAL*4` or `REAL*8`, but their types must agree (at the time of writing).

---------

Author: Masa Sakano, [Wise Babel Ltd](http://www.wisebabel.com); MIT License (see [License.txt](./License.txt))

