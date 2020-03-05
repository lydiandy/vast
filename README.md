## vast

A simple tool for vlang, generate v source file to  AST json file.

so you can see the AST of  v source file.

**NOTE:**

**vast will follow the newest version of V. If you run into an error, please update your version of V by running `v up`.**

## Installation

- via vpm

  ```
  v install lydiandy.vast
  ```

- via source

  ```
  git clone git@github.com:lydiandy/vast.git
  ln -s `pwd`/vast ~/.vmodules/vast
  ```

  now you can run the example code in example directory

  ```
   cd example
   v run ./main.v
  ```

  it will parse the demo.v file and generate demo.json, open it ~

  ![](example/json.png)

## usage

```v
module main

import vast

fn main() {
	file := './demo.v'

	// generate json file with the same name
	vast.json_file(file)

	// or generate ast string
	ast_str:=vast.json(file)
	println(ast_str)
}
```

## todo

- make type from id to name
- make enum from id to name

## License

MIT

## Contributors

pull request is welcome ~
