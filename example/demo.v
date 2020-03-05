//this is comment
module demo
//comment for include
#inclde abc

import (
	os
)

import (
	time 
	term
)
const (
	a=1
	b=3
	c='c'
)

pub struct Point {
	x int
mut: 
	y string
pub:
	z f32
pub mut:
	a string
}

pub enum Color {
	red
	green
	blue	
}

//main funciton
fn main() {
	println(add(1,2))
	println('ok') //comment println
}
//add function
fn add(x,y int) int {
	return x+y
}