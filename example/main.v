module main

import vast

fn main() {
	file:='./demo.v'

	//generate json file with the same name
	vast.json_file(file)

	//or generate ast string
	ast_str:=vast.json(file)
	println(ast_str)
}