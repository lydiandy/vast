//this is comment
module demo
//comment for include
#include abc

import os
import strings

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
	add(1,3)
	println(add(1,2))
	println('ok') //comment println
	arr:=[1,3,5,7]
	for a in arr {
		println(a)
		add(1,3)
	}
}
//add function
fn add(x,y int) int {
	return x+y
}