//comment for module
module main

//comment for import
import os
import strings
import time 

pub const (
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
type Myint int

type MySumType = int | string | bool

type Myfn fn(int) int

// pub interface Myinterfacer {
// 	add(int,int) int
// 	sub(int,int) int
// }

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
	color:=Color.red
	println(color)
}
//add function
fn add(x,y int) int {
	return x+y
}