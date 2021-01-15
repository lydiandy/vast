# vast

A simple tool for vlang, generate v source file to  AST json file.

It will generate example code to AST json file, which can help you understand the AST better.

**NOTE:**

**vast will follow the newest version of V. If you run into an error, please update your version of V by running `v up`.**

# Installation

- via vpm

```shell
v install lydiandy.vast
```

- via source

```shell
git clone git@github.com:lydiandy/vast.git
ln -s `pwd`/vast ~/.vmodules/vast
```

build

```shell
cd vast
v -prod .
```

# Usage

```shell
vast demo.v				//generate example.json and watch example.v.

vast -w demo.v		//generate example.json and watch example.v.

vast -p demo.v		//no generate json file,just print the json string to termial.
```



```shell
./vast ./example/demo.v
```

 it will parse the demo.v file and generate demo.json, open it ~

![](example/json.png)

or you can parse the vlib/builtin/array.v

```shell
vast your/v/dir/vlib/builtin/array.v 
```

it will generate array.json file, ~22000 lines json code.

![](example/array.png)

# Vlang AST struct diagram

![](./ast_struct_diagram.jpg)

# Vlang  AST struct introduction

## Overview

The Vlang abstract syntax tree is implemented by using sum type. 

All the AST struct declarations can be found in V source code: [vlib/v/ast/ast.v](https://github.com/vlang/v/blob/master/vlib/v/ast/ast.v).

## File

example code

```v
module main

import os
import time

fn main() {
  
}
```

##  Module

AST struct

```
Module

Import
```

example code

```v
module main

import os
import time as t
import math { min, max }
```

## Const

AST struct

```
Const
```

example code

```v
module main

const (
	// version comment 1
	version = '0.2.0' // version comment 2
	usage   = 'usage:xxxx'
	pi      = 3.14
	//end comment 1
	//end comment 2
)
```

## Enum

```
EnumDecl

EnumField

EnumVal
```

example code

```v
module main

[attr1]
['attr2:123']
enum Color { // enum comment 1
	// black comment 1
	// black comment 2
	black = 2 // black comment 3
	// white comment 1
	// white comment 2
	white // white comment 3
	blue
	green // green comment
	// end comment 1
	// end comment 2
}

[flag]
enum BitEnum {
	e1
	e2
	e3
}

[_allow_multiple_values]
enum MultipleEnum {
	v1 = 1
}

fn main() {
	mut color := Color.black
	color = .blue
}
```

## Variable

### Assign

AST struct

```
AssignStmt
```

example code

```v
module main

fn main() {
	// an assignment
	a := 'abc' // comment for a
	mut b := 1
	// more operator
	b = 2
	b += 2
	b -= 2
	b *= 2
	b /= 2
	b %= 2
	// multi assign
	x, y, z := 1, 'y', 3.3
	mut xx, mut yy, zz := 1, 3, 5
	// swap variable
	mut c := 1
	mut d := 2
	c, d = d, c
}

```

### Identifier

AST struct

```
Ident

IdentFn

IdentVar
```

example code(todo: need more kind)

```v
module main

fn main() {
	i := 123 // common(unresolved) identifier
	_, x := 1, 2 // blank identifier
	mut s := 'abc' // with mut
	s = 'aaa'
}

```

### Literal

AST struct

```v
IntegerLiteral

FloatLiteral

StringLiteral

StringLiteral

StringInterLiteral

CharLiteral

BoolLiteral
```

example code

```v
module main

fn main() {
	a := 1 // integer literal
	b := 1.2 // float literal
	c := 'abc' // string literal
  name:='tom'
  age:= 33
  //string literal with `$xx` or `${xxx}`
	s1 := 'a is $a,b is $b,c is $c' 
	s2 := 'name is ${name}, age is ${age}'
	e := `c` // char literal
	f := true // bool literal
}
```

### AsCast

AST struct

```v
AsCast
```

example code

```v
module main

type Mysumtype = bool | f64 | int | string

fn main() {
	x := Mysumtype(3)
	x2 := x as int // as must be used for sumtype
	println(x2)
}
```

### SizeOf

AST struct

```v
SizeOf
```

example code

```v
module main

struct Point {
	x int
	y int
}

fn main() {
	a := sizeof(int) // basic type
	b := sizeof(bool) // basic type
	p := Point{
		x: 1
		y: 2
	}
	s1 := sizeof(Point) // struct type
	s2 := sizeof(p) // variable
}
```

### TypeOf

AST struct

```v
TypeOf
```

example code

```v
module main

type MySumType = f32 | int

fn myfn(i int) int {
	return i
}

fn main() {
	a := 123
	s := 'abc'
	aint := []int{}
	astring := []string{}
	println(typeof(a)) // int
	println(typeof(s)) // string
	println(typeof(aint)) // array_int
	println(typeof(astring)) // array_string
	// sumtype
	sa := MySumType(32)
	println(typeof(sa)) // int
	// function type
	println(typeof(myfn)) // fn (int) int
}

```

### CastExpr

AST struct

```v
CastExpr
```

example code ( todo: need more about string(buf,n) )

```v
module main

fn main() {
	x:=byte(3)
	y:=f32(2.1)
}
```

## Array

### ArrayInit

AST struct

```v
ArrayInit
```

example code

```v
module main

fn main() {
	mut arr := []string{len: 3, cap: 6, init: 'default'}
	arr[0] = 'a'
	arr[1] = 'b'
	println(arr)
}
```

### IndexExpr

AST struct

```v
IndexExpr
```

example code

```v
module main

fn main() {
	mut arr := []string{len: 3, cap: 6, init: 'default'}
	arr[0] = 'a' //index expr
	arr[1] = 'b'
	println(arr)
  mut m := map[string]string{}
	m['name'] = 'tom' //index expr
	m['age'] = '33'
}
```

### RangeExpr

AST struct

```v
RangeExpr
```

example code

```v
module main

fn main() {
	n := [1, 2, 3, 4, 5]
	a1 := n[..2] //[1, 2]
	a2 := n[2..] //[3, 4, 5]
	a3 := n[2..4] //[3, 4]
}
```

### ArrayDecompose

AST struct

```v
ArrayDecompose
```

example code

```v
module main

fn main() {
	a := ['a', 'b', 'c'] 
	println(variadic_fn_a(a...)) //ArrayDecompose
}

fn variadic_fn_a(a ...string) string {
	return variadic_fn_b(a...) //ArrayDecompose
}

fn variadic_fn_b(a ...string) string {
	a0 := a[0]
	a1 := a[1]
	a2 := a[2]
	return '$a0$a1$a2'
}

```

## Map

### MapInit

AST struct

```v
MapInit
```

example code

```v
module main

fn main() {
	mut m := map[string]string{} //map declaration
	m['name'] = 'tom'
	m['age'] = '33'
	//map literal declaration and init
	m2 := {
		'one':   1
		'two':   2
		'three': 3
	}
}
```

## Operator

### PrefixExpr

AST struct

```v
PrefixExpr
```

example code

```v
module main

fn main() {
	x := -1 // minus
	p := &x // get address of variable
	x2 := *p // get value of pointer
	b := !true // logic not
	bit := ~0x0000 // bit not
}

```

### InfixExpr

AST struct

```v
InfixExpr
```

example code

```v
module main

fn main() {
	x := 1 + 2
	y := 1 - 2
	a := x == y // equal
	b := x > y // compare
	c := 1 in [1, 2] // in operator
	d := (x > y) && (1 < 2) // logic and
	e := 2 == 2 || 3 == 3 // logic or
	mut arr := [1, 2] // array append
	arr << 3
}
```

### PostfixExpr

AST struct

```v
PostfixExpr
```

example code

```v
module main

fn main() {
	mut x:=1
	x++
	x--
}

```

### SelectorExpr

AST struct

```v
SelectorExpr
```

example code

```v
module main

struct Point {
mut:
	x int
	y int
}

fn (mut p Point) move(a int, b int) {
	p.x += a // selector for struct field assign
	p.y += b
}

fn main() {
	mut p := Point{
		x: 1
		y: 3
	}
	p.x // selector for access field value
	p.move(2, 3)
}

```

### ParExpr

AST struct

```v
ParExpr
```

example code

```v
module main

fn main() {
	x:=(1+2)
	y:=(1<2)
}

```

### ConcatExpr

AST struct

```v
ConcatExpr
```

example code

```v
a, b, c := match false {
	true { 1, 2, 3 }
	false { 4, 5, 6 }
	else { 7, 8, 9 }
}
```

## Function

### FnDecl

AST struct

```v
FnDecl

CallExpr

CallArg

Return
```

example code

```v
module main

fn main() {
	s := add(1, 3)
	println(s)
	s2 := add_generic(2, 4)
	s3 := add_generic<int>(2, 4)
	println(s2)
	println(s3)
}

// function
fn add(x int, y int) int {
	return x + y
}

struct Point {
	x int
	y int
}

// method
pub fn (p Point) move(a int, b int) (int, int) {
	new_x := p.x + a
	new_y := p.y + b
	return new_x, new_y
}

// generic function
fn add_generic<T>(x T, y T) T {
	return x + y
}
```

### AnonFn

AST struct

```v
AnonFn
```

example code

```v
module main

fn main() {
	f1 := fn (x int, y int) int {
		return x + y
	}
	f1(1,3)
}
```

### DeferStmt

AST struct

```v
DeferStmt
```

example code

```v
fn main() {
	println('main start')
	// defer {defer_fn1()} 
	// defer {defer_fn2()}
	defer {
		defer_fn1()
	}
	defer {
		defer_fn2()
	}
	println('main end')
}

fn defer_fn1() {
	println('from defer_fn1')
}

fn defer_fn2() {
	println('from defer_fn2')
}
```

## Struct

### StructDecl

AST struct

```v
StructDecl

StructField

Embed
```

example code

```v
module main

[attr1]
[attr2]
struct Point { //comment 1
mut:
	x int [attr3]
	y int ['attr4=123']
pub mut:
	z int = 1
//end comment
}

fn main() {
}
```

example code of  embed

```v
module main

struct Widget {
mut:
	x int
	y int
}

pub fn (mut w Widget) move(x_step int, y_step int) {
	w.x += x_step
	w.y += y_step
}

struct Widget2 {
mut:
	z int
}

pub fn (mut w Widget2) move_z(z_step int) {
	w.z += z_step
}

struct Button {
	Widget //embed
	Widget2 //embed
	title string
}

fn main() {
	mut button := Button{
		title: 'Click me'
	}
	button.x = 3 // x comes from Widget
	button.z = 4 // z comes from Widget2
	println('x:$button.x,y:$button.y,z:$button.z')
	button.move(3, 4) // move comes from Widget
	println('x:$button.x,y:$button.y,z:$button.z')
	button.move_z(5) // move_z comes from Widget2
	println('x:$button.x,y:$button.y,z:$button.z')
}
```

### StructInit

AST struct

```v
StructInit

StructInitField

StructInitEmbed
```

example code 

```v
module main

struct User {
	name string
	age int
}

fn add(u User) {
	println(u)
}

fn main(){
	add(User{name:'jack',age:22}) //standard
	add({name:'tom',age:23}) //short
	add(name:'tt',age:33) // more short
}
```

### Assoc

AST struct

```v
Assoc
```

example code

```v
struct Foo {
	a int
	b int
	c int = 7
}

fn main() {
	foo := Foo{
		a: 1
		b: 33
	}
	//associate
	foo2 := {
		foo |
		a: 42
		b: 10
	}
	println(foo2.a) // 42
	println(foo2.b) // 10
	println(foo2.c) // 7
}

```

## Interface

AST struct

```v
InterfaceDecl
```

example code

```v
module main

interface Speaker { //comment 1
	speak() string
	silent()
}
```

## Type

### Alias Type

AST struct

```v
//Alias type declaration
pub struct AliasTypeDecl {
pub:
	name        string
	is_pub      bool
	parent_type table.Type
	pos         token.Position
	comments    []Comment
}
```

example code

```v
module main

struct Human {
	name string
}
type Myint =  int /*comment 1*/ //comment 2
type Person = Human
```

### Function Type

AST struct

```v
FnTypeDecl
```

example code

```v
module main

type Mid_fn = fn (int, string) int /*comment 1*/ //comment 2
```

### Sum  type

AST struct

```v
SumTypeDecl
```

example code

```v
module main

struct User {
	name string
	age  int
}

type MySumtype = User | int | string //comment 1
```

## FlowControl

### Block

AST struct

```v
Block
```

example code

```v
fn main() {
	my_fn()
}

fn my_fn() {
	// block
	{
		println('in block')
	}
	// unsafe block
	unsafe {
	}
}
```

### if

AST struct

```v
IfExpr

IfBranch
```

example code

```v
module main

fn main() {
	a := 10
	b := 20
	// if statement
	if a < b {
		println('$a < $b')
	} else if a > b {
		println('$a > $b')
	} else {
		println('$a == $b')
	}
	// if expr
	num := 777
	s := if num % 2 == 0 { 'even' } else { 'odd' }
	x, y, z := if true { 1, 'awesome', 13 } else { 0, 'bad', 0 }
	// compile time if 
	$if macos {
	} $else {
	}
}
```

### match

AST struct

```v
MatchExpr

MatchBranch
```

example code

```v
fn main() {
	os := 'macos'
	// match statement
	match os {
		'windows' { println('windows') }
		'macos', 'linux' { println('macos or linux') }
		else { println('unknow') }
	}
	// match expr
	price := match os {
		'windows' { 100 }
		'linux' { 120 }
		'macos' { 150 }
		else { 0 }
	}
	// multi assign 
	a, b, c := match false {
		true { 1, 2, 3 }
		false { 4, 5, 6 }
		else { 7, 8, 9 }
	}
}

type MySum = bool | int | string

pub fn (ms MySum) str() string {
	// match sum type
	match ms {
		int { return ms.str() }
		string { return ms }
		else { return 'unknown' }
	}
}
```

### for

AST struct

```v
ForCStmt

ForInStmt

ForStmt

BranchStmt
```

example code

```v
fn main() {
	for i := 0; i < 10; i++ {
		if i == 6 {
			continue
		}
		if i == 10 {
			break
		}
		println(i)
	}
}
```

example code

```v
fn main() {
	// string
	str := 'abcdef'
	for s in str {
		println(s.str())
	}
	// array
	numbers := [1, 2, 3, 4, 5]
	for num in numbers {
		println('num:$num')
	}
	// range
	mut sum := 0
	for i in 1 .. 11 {
		sum += i
	}
	// map
	m := {
		'name': 'jack'
		'age':  '20'
		'desc': 'good man'
	}
	for key, value in m {
		println('key:$key,value:$value')
	}
}
```

example code

```v
fn main() {
	mut sum := 0
	mut x := 0
	for x <= 100 {
		sum += x
		x++
	}
	println(sum)
	// label for
	mut i := 4
	goto L1
	L1: for { // label for
		i++
		for {
			if i < 7 {
				continue L1
			} else {
				break L1
			}
		}
	}
}
```

### goto

AST struct

```v
GotoLabel

GotoStmt
```

example code

```v
fn main() {
	mut i := 0
	a: // goto label
	i++
	if i < 3 {
		goto a
	}
	println(i)
}
```

## Error handle

AST struct

```v
OrExpr

None
```

example code

```v
fn my_fn(i int) ?int {
	if i == 0 {
		return error('Not ok!')
	}
	if i == 1 {
		return none
	}
	return i
}

fn main() {
	println('from main') // OrKind is absent
	v1 := my_fn(0) or { // OrKind is block
		println('from 0')
		panic(err)
	}
	v2 := my_fn(1) or { 
		println('from 1') 
		panic('error msg is $err')	
	}
	v3 := my_fn(2) or {
		println('from 2')
		return
	}
	v4 := my_fn(3) ? // OrKind is propagate
}

```

## Concurrent

### ChanInit

AST struct

```v
ChanInit

GoStmt
```

example code

```v
module main

const (
	num_iterations = 10000
)

fn do_send(ch chan int) {
	for i in 0 .. num_iterations {
		ch <- i
	}
}

fn main() {
	ch := chan int{cap: 1000} // chan init
	go do_send(ch) // go statement
	mut sum := i64(0)
	for _ in 0 .. num_iterations {
		sum += <-ch
		println(sum)
	}
}
```

### SelectExpr

AST struct

```v
SelectExpr

SelectBranch
```

example code

```v
import time
import sync

fn main() {
	ch1 := chan int{}
	ch2 := chan int{}
	go send(ch1, ch2)
	mut x := 0
	mut y := 0
	for {
		select { // 
			x = <-ch1 { // read channel
				println('$x')
			}
			y = <-ch2 {
				println('$y')
			}
			> 2 * time.second { // timeout
				break
			}
		}
	}
}

fn send(ch1 chan int, ch2 chan int) {
	ch1 <- 1
	ch2 <- 2
	ch1 <- 3
	ch2 <- 4
	ch1 <- 5
	ch2 <- 6
}
```

### LockExpr

AST struct

```v
LockExpr
```

example code(todo)

```v

```

## Unsafe

AST struct

```v
UnsafeExpr
```

example code

```v
module main

fn main() {
	a := ['a', 'b', 'c']
	p := unsafe { &a[2] } // unsafe expr
	println(p)
}
```

## SQL

AST struct

```v
SqlStmt

SqlExpr
```

example code

```v
module main

import sqlite

struct Module {
	id           int
	name         string
	nr_downloads int
}

struct User {
	id             int
	age            int
	name           string
	is_customer    bool
	skipped_string string [skip]
}

struct Foo {
	age int
}

fn main() {
	db := sqlite.connect(':memory:') or { panic(err) }
	db.exec('drop table if exists User')
	db.exec("create table User (id integer primary key, age int default 0, name text default '', is_customer int default 0);")
	name := 'Peter'
	db.exec("insert into User (name, age) values ('Sam', 29)")
	db.exec("insert into User (name, age) values ('Peter', 31)")
	db.exec("insert into User (name, age, is_customer) values ('Kate', 30, 1)")
	nr_all_users := sql db {
		select count from User
	}
	println('nr_all_users=$nr_all_users')
	//
	nr_users1 := sql db {
		select count from User where id == 1
	}
	println('nr_users1=$nr_users1')
	//
	nr_peters := sql db {
		select count from User where id == 2 && name == 'Peter'
	}
	//
	new_user := User{
		name: 'New user'
		age: 30
	}
	sql db {
		insert new_user into User
	}
	sql db {
		update User set age = 31 where name == 'Kate'
	}
	sql db {
		delete from User where age == 34
	}
}

```

## Test

### AssertStmt

AST struct

```v
AssertStmt
```

example code

```v
fn test_abc() {
	x := 1
	assert x == 1
}
```

## Compile time

### CompFor

AST struct

```v
CompFor

ComptimeCall
```

example code

```v
struct App {
	a string
	b string
mut:
	c int
	d f32
pub:
	e f32
	f u64
pub mut:
	g string
	h byte
}

fn (mut app App) m1() {
}

fn (mut app App) m2() {
}

fn (mut app App) m3() int {
	return 0
}

fn main() {
	$for field in App.fields {
		println('field: $field.name')
	}
	$for method in App.methods {
		println('method: $method.name')
	}
}

```

## C Integration

### GlobalDecl

AST struct

```v
GlobalDecl

GlobalField
```

example code

```v
module main

// single
__global ( g1 int )

// group
__global (
	g2 byteptr 
	g3 byteptr 
)

fn main() {
	g1 = 123
	println(g1)
	println(g2)
	println(g3)
}
```

### HashStmt

AST struct

```v
HashStmt
```

example code

```v
module main
    
#include <stdio.h> 

#flag -lmysqlclient
#flag linux -I/usr/include/mysql
#include <mysql.h>

fn main() {

}
```

### Likely

AST struct

```v
Likely
```

example code

```v
module main

fn main() {
	x := 1
	if _likely_(x == 1) {
		println('a')
	} else {
		println('b')
	}
	if _unlikely_(x == 1) {
		println('a')
	} else {
		println('b')
	}
}
```

### CTempVar

AST struct

```v
CTempVar
```

example code(todo)

```v

```

## Comment

AST struct

```v
Comment
```

example code

```v
module main
/*
multi line comment
multi line comment
*/
// signle line comment
fn main() {
	x := 1 // behind statement comment
}
```

## Other

### AtExpr

AST struct

```v
AtExpr
```

example code

```v
module main

fn main() {
	println(@MOD)
	println(@FN)
	println(@STRUCT)
	println(@VEXE)
	println(@FILE)
	println(@LINE)
	println(@COLUMN)
	println(@VHASH)
	println(@VMOD_FILE)
}
```



# License

MIT

# Contributors

pull request is welcome ~
