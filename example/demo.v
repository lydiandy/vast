// comment for module
module main

//import module
import os
import math

//const decl
const (
	a = 1
	b = 3
	c = 'c'
)
//struct decl
struct Point {
	x int
mut:
	y string
pub:
	z f32
pub mut:
	a string
}

// method of Point
pub fn (p Point) get_x() int {
	return p.x
}
//enum type
enum Color {
	red
	green
	blue
}
//type alias
type Myint = int

//sum type
type MySumType = bool | int | string

//function type
type Myfn = fn (arg_1 int) int

//interface type
interface Myinterfacer {
	add(int, int) int
	sub(int, int) int
}

// main funciton
fn main() {
	add(1, 3)
	println(add(1, 2))
	println('ok') // comment println
	arr := [1, 3, 5, 7]
	for a in arr {
		println(a)
		add(1, 3)
	}
	color := Color.red
	println(color)
	println(os.args)
	m := math.max(1, 3)
	println(m)
}

// normal function
fn add(x int, y int) int {
	return x + y
}

// generic function
fn g_fn<T>(p T) T {
	return p
}

// generic struct
struct GenericStruct<T> {
	point    Point
mut:
	model T
}
