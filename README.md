# budc

Bud is a C-like language that takes some basic things from Rust. Bud is my attempt at making a language/compiler, and budc compiles to 68k Assembly, the only assembly language I know and the assembly language of my Microcomputer Design project. See the software for that project [here](https://github.com/Budmeister/MCD-68k-C). 

## Similarities to C
1. Low-level
2. Manual memory management
3. No runtime types
4. Compatible with C (specifically, [Ashware's 68k C compiler](https://www.ashware.com/gnu-68kcoldfire-cc-compiler) from 1999)
5. Arguments are passed by value (even arrays)

## Differences from C
1. Like in Rust, in Bud, there are no statements, only expressions. An expression followed by a semicolon returns `void`, a zero-sized type. `if else` is also an expression. The last expression in a block expression is its return value.

```
i32 a = 4;
i32 b = 5;
i32 greater = if a > b { a } else { b };
```

2. I took my own approach to array type definition. In C, if you want a 2-D array with 3 rows and 4 columns, you would write,

```
int a[3][4];
```
  The reasoning goes, _you will index `a` with up to 3 and then index that with up to 4 and end up with an `int`_.
  
  But in Bud, you would write,
```
i32[4][3] a;
```
  The reasoning goes, _you start with an `i32`; add `[4]` to make it an array of 4 `i32`s. Then add `[3]` to make it an array of 3 arrays of 4 `i32`s._

3. There are two ways to index into an array. If you use square brackets, `a[2]`, there will be no runtime bounds-checks, as in C. But if you use round brackets, `a(2)` (yes that's what they're called), there will be a runtime bounds-check where 68k-bounds-check exception will occur if the index is out of bounds. If you index with a literal int that is out of bounds, then if you used square brackets, you will get a compile-time warning, and if you used round brackets, you will get a compile-time error.
4. If I had finished implementing structs, you would have indexed into them like this: `my_struct[fieldname]`. This is just because structs feel a lot like arrays that can return different types.
5. There are no local scopes. All local variables exist for the duration of the function.
6. There is no order of operations. All binary operators are carried out left-to-right.

## Other Important Features
1. You can declare externally defined functions and global variables using the `extern` keyword.
```
extern void printf(@(i8) str);
extern i32 variable;
extern {
  @(void) malloc(i32 len);
  i32 other_extern_variable;
}
```
2. Casting: You can implicitly cast between any two types as long as they are the same size. You will get a compile-time warning when you do this unless the types are both pointers (of the form, `@(F)` casting to `@(T)`) and one of the following conditions is met.
   * `F == void`
   * `T == void`
   * `F == T[_][_]...`
   * `T == F[_][_]...`
3. Arrays: Array literals are written like `[[1, 2, 3, 4]]` and can be used anywhere (not just at the time of assignment) as an expression.

## Tested Features
1. `printf` (and therefore `extern`)
2. Local variables
3. Pointers (referencing and dereferencing)
4. Addition, Subtraction, Multiplication
5. `if`, `while`, `unless`, `do while`
6. Global variables
7. Arrays
8. Returning values from a function

## Untested Features
1. Division
2. Automatic bounds checking
3. I know that structs currently do not work
4. 2-D array literals

## Un-implemented/Partially Implemented Features
1. Importing a Bud file. The item, `import ./path/to/file.bud;` should parse that file and include all its functions and global variables as extern in this file.
2. Returning arrays or structs from a function
3. Explicit casting
4. Comments

## Just Because
I added in a few things just because I thought they were interesting. 
1. The reference operator: The reference operator is `@( )`. Pointer arithmetic is not implemented yet.
2. The `cleanup` expression: The `cleanup` expression allows you to exit early from a function but to do some cleanup before you return. If you use it, it must be defined at the end of a function. If control reaches the cleanup block, cleanup will occur, but if you call `return` before that point, the cleanup is skipped.
```
i32 function_that_can_fail(i32 index) {
  if i_feel_like_it() {
    // This value gets returned as it is the last expression in this block
    index * 2
  } else {
    // So does this one
    FAILURE_VALUE
  }
}

i32 calculate(i32 size) {
  @(i8) buf = malloc(size);

  i32 index = 0;
  while index < size {
    // This variable exists for the whole function
    i32 value = function_that_can_fail(index);
    if value == FAILURE_VALUE {
      cleanup;
    }
    buf[index] = value;
  }

  // Processing...

  cleanup {
    free(buf);
  }
}
```
2. The `empty` expression: It's an expression that does nothing and can fit as any type. In cases where the value would immediately be pushed onto the stack, it just moves the stack pointer the appropriate number of bytes so that it is _always_ safe to use. `empty` is necessary if you want to declare a variable without assigning it right away
```
i32 val = empty;
```
or write an `if` statement without a body.
```
if a < 3 {
  empty
}
```
This is because my parser cannot handle epsilon transitions and would fail if you wrote this.
```
if a < 3 {

}
```
