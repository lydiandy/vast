// comment for module
module main

import os
import math

const (
	a = 1
	b = 3
	c = 'c'
)

struct Point {
	x int
mut:
	y string
pub:
	z f32
pub mut:
	a string
}

enum Color {
	red
	green
	blue
}

type Myint int

type MySumType = bool | int | string

type Myfn = fn (arg_1 int) int

interface Myinterfacer {
add(int,int) int
sub(int,int) int
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

// add function
fn add(x, y int) int {
	return x + y
}
