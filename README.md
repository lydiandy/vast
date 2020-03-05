## vast

A simple tool for vlang, generate v source file to  AST json file.

so you can see the AST of  v source file.

**NOTE:**

**the vast will follow the newest version of V, If you meet error,please update the V source code from:https://github.com/vlang/v**

## Installation

- via vpm

  ```
  v install lydiandy.vast
  ```

- via source

  ```
  git clone git@github.com:lydiandy/vast.git
  ln -s your/path ~/.vmodules/vast
  ```

  now you can run the example code in example directory

  ```
   cd example
   v run ./main.v
  ```

  it will parse the demo.v file and generate demo.json, open it ~


## usage

```v
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
```

## License

MIT

## Contributors

pull request is welcome ~